{
  description = "Caching gitlab issues and more locally, for bazingly fast search";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "local-gitlab";

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

        asdf-system = pkgs.lispPackages_new.build-asdf-system asdf-system-attrs;

        application = pkgs.lispPackages_new.build-asdf-system asdf-system-attrs // {
          buildScript = pkgs.writeText "build-${name}.lisp" ''
          (require :asdf)
          (asdf:load-system '#:${name})
          (sb-ext:save-lisp-and-die "${name}"
            :executable t
            :toplevel #'${name}:main)
         '';

          nativeBuildInputs = [ pkgs.makeWrapper ];
          installPhase = ''
          source $stdenv/setup
          mkdir -p $out/bin
          mv ${name} $out/bin
          wrapProgram $out/bin/${name} \
            --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
          '';
        };

        sbclWithPackages = (pkgs.lispPackages_new.sbclWithPackages
          (p: [
            asdf-system
            p.swank
            p.slynk
          ]));

        start-swank-listener = pkgs.writeScriptBin "swank"
          ''
          ${pkgs.rlwrap}/bin/rlwrap ${sbclWithPackages}/bin/sbcl --noinform \
            --eval "(asdf:load-system '#:${name})" \
            --eval "(asdf:load-system '#:swank)" \
            --eval "(swank:create-server :dont-close t)"
          '';
      in {
        packages.default = application;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbclWithPackages
            pkgs.rlwrap
            application
            start-swank-listener
          ];
        };
      }
    );
}
