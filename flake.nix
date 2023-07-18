{
  description = "Caching gitlab issues and more locally, for bazingly fast search";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "cache-cache";

        pkgs = nixpkgs.legacyPackages.${system};

        lisp = "${pkgs.sbcl}/bin/sbcl --script";

        # This is a project not in quicklisp, and the version in nixpkgs is too old
        jzon = pkgs.lispPackages_new.build-asdf-system {
          src = pkgs.fetchgit {
            url = "https://github.com/Zulu-Inuoe/jzon.git";
            rev = "ba43faa1f2a07e83226d0e52b29cf6a816e3596d";
            sha256 = "sha256-MWitSH1vgszhlW0EgfPX/7lZHxdXZYYqhwWqrpaUWFc=";
          };
          version = "1.0.0-2021109-ba43faa";
          pname = "jzon";
          inherit lisp;
          lispLibs = with pkgs.lispPackages_new.sbclPackages; [closer-mop flexi-streams];
          systems = [ "com.inuoe.jzon" ];
        };

        # This another of my projects, it's not on quicklisp
        simpbin = pkgs.lispPackages_new.build-asdf-system {
          pname = "simpbin";
          version = "0.0.1";
          src = pkgs.fetchgit {
            url = "https://github.com/fstamour/simpbin.git";
            rev = "6f9f1c196ca8f363b478bab0a8623f53b89e5586"; # Master as of 9th Nov. 2022
            sha256 = "sha256-FLCUYbC/oMpCqQNGo5Bh3UZyxYx4/Jm8VtXKuqtC5uA=";
          };
          inherit lisp;
          lispLibs = with pkgs.lispPackages_new.sbclPackages; [alexandria flexi-streams fast-io nibbles];
        };

        lispLibs =  with pkgs.lispPackages_new.sbclPackages;
          [
            drakma
            log4cl
            hunchentoot
            find-port
            kebab
            str
            local-time
            cl-cron
            adopt

            jzon
            simpbin
          ];

        meta = {
          homepage = "https://github.com/fstamour/${name}";
          description = "Caching gitlab issues and more locally, for bazingly fast search";
          license = pkgs.lib.licenses.mit;
          maintainers = [ pkgs.maintainers.mpsyco ];
        };

        asdf-system-attrs = {
          pname = "${name}";
          version = "0.0.1";

          src = ./.;
          inherit lisp;
          inherit lispLibs;
          inherit meta;
        };

        # A derivation for the lisp system
        asdf-system = pkgs.lispPackages_new.build-asdf-system asdf-system-attrs;

        # A derivation for an executable for the lisp system
        application = pkgs.lispPackages_new.build-asdf-system asdf-system-attrs // {
          # Load the system, specify a top-level function and dump the
          # executable core.
          buildScript = pkgs.writeText "build-${name}.lisp" ''
          (require :asdf)
          (asdf:load-system '#:${name})
          (uiop/image:dump-image "${name}" :executable t)
         '';

          # To make runtime depedencies (like openssl) available.
          nativeBuildInputs = [ pkgs.makeWrapper ];

          # Wrap the executable, keep only the executable as output.
          installPhase = ''
          source $stdenv/setup
          mkdir -p $out/bin
          mv ${name} $out/bin
          wrapProgram $out/bin/${name} \
            --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
          '';
        };


        # A version of sbcl with some systems for development.
        sbclWithPackages = (pkgs.lispPackages_new.sbclWithPackages
          (p: [
            asdf-system
            p.swank
            p.slynk
          ]));

        # A small script to launch sbcl with swank started and the system loaded.
        # TODO we probably won't need this if we configure emacs to start sbcl
        start-swank-listener = pkgs.writeScriptBin "swank"
          ''
          ${pkgs.rlwrap}/bin/rlwrap ${sbclWithPackages}/bin/sbcl --noinform \
            --eval "(asdf:load-system '#:${name})" \
            --eval "(asdf:load-system '#:swank)" \
            --eval "(swank:create-server :dont-close t)"
          '';

        emacs-setup-slime = pkgs.writeTextFile {
          name = "setup-slime.el";
          text = ''
           (let ((slime-directory "${pkgs.lispPackages_new.sbclPackages.swank}"))
             (add-to-list 'load-path slime-directory)
             (require 'slime-autoloads)
             (setq slime-backend (expand-file-name "swank-loader.lisp" slime-directory))
             (setq slime-path slime-directory)
             (slime-setup '(slime-fancy)))

            (setq slime-lisp-implementations
             `((sbcl ("${sbclWithPackages}/bin/sbcl" "--no-sysinit" "--no-userinit"
                     "--eval" "(require 'asdf)"))))

          '';
        };

        # Small script to load slime (swank's emacs client) using emacsclient
        swank-emacsclient = pkgs.writeScriptBin "swank-emacsclient"
          ''
          ${pkgs.emacs}/bin/emacsclient -n --eval "(load \"${emacs-setup-slime}\")"
          '';
      in {
        packages.default = application;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbclWithPackages
            pkgs.rlwrap
            application
            start-swank-listener
            swank-emacsclient
          ];
        };
      }
    );
}
