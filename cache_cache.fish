#!/usr/bin/env fish
#
# Uses cache-cache's server, curl, jq and fzf to quickly find issues and project
#

set self (status --current-filename)

# curl http://localhost:40000/issues -s | jq keys

# I'm keeping those 2 because they are simpler and fuzzier
# Not used
function list-issues
    curl http://localhost:40000/issues -s | jq -r '.[] | "\(.id) \(.text)"'
end

# Not used
function select-issue
    set -l id (list-issues | fzf --preview $self' --preview (echo {} | cut -d \\t  -f 1)' | cut -d \t -f 1)
    or exit 1
    echo $id
end



function search
    set argv (string split ' ' "$argv[1]")
    set -l type_p $argv[1]
    echo "type_p: $type_p" >&2

    switch $type_p
        case '\p'
            set type_p "project"
        case '\i'
            set type_p "issue"
    end

    switch $type_p
        case project issue
            set -e argv[1]
            set type "type=$type_p&"
        case '*'
            set type ""
    end

    set -l query "q="(string escape --style=url (string trim "$argv"))
    set -l url "http://localhost:40000/search?$type$query"
    echo "type: $type" >&2
    echo "query: $query" >&2
    echo "search url: $url" >&2
    curl -s $url \
        | jq -r 'map([.id, .text] | join("\t")) | .[]'
end

function get
    set -l id $argv[1]
    if [ "$id" = '' ]
        echo "get: must specify an id" >&2
        exit 1
    end
    curl -s "http://localhost:40000/item?id=$id" | jq .
end


function search-issue
    set -l query (echo '' | fzf --print-query  --preview $self' --search {q}' --preview-window=up:95%)
    if [ "$query" = '' ]
        return 1
    end

    set -l id (search $query | fzf -1 --preview $self' --preview (echo {} | cut -d \\t -f 1)' | cut -d \t -f 1)
    or exit 1
    echo "id: $id"

    set -l url (get $id | jq -r '.web_url')
    echo "url: $url"
    xdg-open $url
end

function preview
    set -l id $argv[1]
    set -l json (get $id)
    or exit 1

    # set -l title (echo $json | jq -r '(.title)')
    echo $json | jq -r '.title'
    # echo $title
    # echo $json | jq 'keys'
    echo
    echo $json | jq -r .web_url
    echo
    echo $json | jq -r .description
end

# This works, but is it too slow...
function fuzzy_highlight
    set -l needle (string lower (echo $argv[1] | tr -d ' '))
    set -l needle_length (string length $needle)

    while read --local --line input
        set -l i 1
        set -l input_length (string length $input)

        set -l j 1
        set -l max_length (math "max($input_length, $needle_length)")

        set -l k 1
        for k in (seq $max_length)
            set -l input_char (string sub -s $i -l 1 $input)
            set -l needle_char (string sub -s $j -l 1 $needle)

            if [ (string lower "$input_char") = "$needle_char" ]
                set_color red
                printf "$input_char"
                set_color normal
                set j (math $j + 1)
                if [ "$j" -gt "$needle_length" ]
                    set i (math $i + 1)
                    break
                end
            else
                printf "$input_char"
            end
            set i (math $i + 1)
        end

        echo (string sub -s (math "$k + 1") $input)
    end
end

argparse "filter=" "preview=" "search=" -- $argv

if set -q _flag_filter
    echo "deprecated" >&2
    exit 1
    fuzzy_highlight "$_flag_filter"
    exit 0
end

if set -q _flag_preview
    echo "Preview $_flag_preview:"
    preview "$_flag_preview"
    exit $status
end

if set -q _flag_search
    search "$_flag_search"
    exit $status
end

# select-issue
search-issue


# echo quicksilver | fuzzy_highlight qik
