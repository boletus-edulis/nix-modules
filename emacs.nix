{ config, pkgs, lib, ... }:

{
  programs.nushell = {
    enable = true;
    plugins = [
      pkgs.nushellPlugins.formats
      pkgs.nushellPlugins.query
      pkgs.nushellPlugins.polars
    ];
    configFile = {
      text = ''
def color_theme [] {
    return {
        binary: '#747474'
        block: '#686868'
        cell-path: '#b9b9b9'
        closure: '#868686'
        custom: '#f7f7f7'
        duration: '#a0a0a0'
        float: '#7c7c7c'
        glob: '#f7f7f7'
        int: '#747474'
        list: '#868686'
        nothing: '#7c7c7c'
        range: '#a0a0a0'
        record: '#868686'
        string: '#8e8e8e'

        bool: {|| if $in { '#868686' } else { '#a0a0a0' } }

        datetime: {|| (date now) - $in |
            if $in < 1hr {
                { fg: '#7c7c7c' attr: 'b' }
            } else if $in < 6hr {
                '#7c7c7c'
            } else if $in < 1day {
                '#a0a0a0'
            } else if $in < 3day {
                '#8e8e8e'
            } else if $in < 1wk {
                { fg: '#8e8e8e' attr: 'b' }
            } else if $in < 6wk {
                '#868686'
            } else if $in < 52wk {
                '#686868'
            } else { 'dark_gray' }
        }

        filesize: {|e|
            if $e == 0b {
                '#b9b9b9'
            } else if $e < 1mb {
                '#868686'
            } else {{ fg: '#686868' }}
        }

        shape_and: { fg: '#747474' attr: 'b' }
        shape_binary: { fg: '#747474' attr: 'b' }
        shape_block: { fg: '#686868' attr: 'b' }
        shape_bool: '#868686'
        shape_closure: { fg: '#868686' attr: 'b' }
        shape_custom: '#8e8e8e'
        shape_datetime: { fg: '#868686' attr: 'b' }
        shape_directory: '#868686'
        shape_external: '#868686'
        shape_external_resolved: '#868686'
        shape_externalarg: { fg: '#8e8e8e' attr: 'b' }
        shape_filepath: '#868686'
        shape_flag: { fg: '#686868' attr: 'b' }
        shape_float: { fg: '#7c7c7c' attr: 'b' }
        shape_garbage: { fg: '#FFFFFF' bg: '#FF0000' attr: 'b' }
        shape_glob_interpolation: { fg: '#868686' attr: 'b' }
        shape_globpattern: { fg: '#868686' attr: 'b' }
        shape_int: { fg: '#747474' attr: 'b' }
        shape_internalcall: { fg: '#868686' attr: 'b' }
        shape_keyword: { fg: '#747474' attr: 'b' }
        shape_list: { fg: '#868686' attr: 'b' }
        shape_literal: '#686868'
        shape_match_pattern: '#8e8e8e'
        shape_matching_brackets: { attr: 'u' }
        shape_nothing: '#7c7c7c'
        shape_operator: '#a0a0a0'
        shape_or: { fg: '#747474' attr: 'b' }
        shape_pipe: { fg: '#747474' attr: 'b' }
        shape_range: { fg: '#a0a0a0' attr: 'b' }
        shape_raw_string: { fg: '#f7f7f7' attr: 'b' }
        shape_record: { fg: '#868686' attr: 'b' }
        shape_redirection: { fg: '#747474' attr: 'b' }
        shape_signature: { fg: '#8e8e8e' attr: 'b' }
        shape_string: '#8e8e8e'
        shape_string_interpolation: { fg: '#868686' attr: 'b' }
        shape_table: { fg: '#686868' attr: 'b' }
        shape_vardecl: { fg: '#686868' attr: 'u' }
        shape_variable: '#747474'

        foreground: '#b9b9b9'
        background: '#101010'
        cursor: '#b9b9b9'

        empty: '#686868'
        header: { fg: '#8e8e8e' attr: 'b' }
        hints: '#525252'
        leading_trailing_space_bg: { attr: 'n' }
        row_index: { fg: '#8e8e8e' attr: 'b' }
        search_result: { fg: '#7c7c7c' bg: '#b9b9b9' }
        separator: '#b9b9b9'
    }
}

$env.config.color_config = (color_theme)
$env.LS_COLORS = ""
$env.config.buffer_editor = [ "emacs" "-nw" ]

$env.PROMPT_COMMAND = {||
    let exit_code = $env.LAST_EXIT_CODE
    let dir = match (do -i { $env.PWD | path relative-to $nu.home-dir }) {
        null => $env.PWD
        "" => '~'
        $relative_pwd => ([~ $relative_pwd] | path join)
    }

    let user = whoami
    let user_color = ansi light_gray
    let user_segment = $"($user_color)($user)(ansi reset)"

    let path_color = ansi light_gray
    let path_segment = $"($path_color)($dir)(ansi reset)"

    let separator_color = ansi light_gray
    let prompt_0 = $path_segment | str replace --all (char path_sep) $"($separator_color)(char path_sep)($path_color)"
    mut prompt_1 = ""
    if $exit_code != 0 {
        prompt_0 | prepend $"(($env.LAST_EXIT_CODE | format number --no-prefix).display):"
    }
    prompt_0 | prepend $"($user_segment):"
}

$env.PROMPT_COMMAND_RIGHT = ""

$env.PROMPT_MULTILINE_INDICATOR = {||
    (if (is-admin) {
        $"(ansi red)::: (ansi reset)"
    } else {
        $"(ansi light_gray)::: (ansi reset)"
    })
}

$env.PROMPT_INDICATOR = {||
    (if (is-admin) {
        $"(ansi red)# (ansi reset)"
    } else {
        $"(ansi light_gray)$ (ansi reset)"
    })
}

$env.TRANSIENT_PROMPT_COMMAND = $env.PROMPT_COMMAND

$env.config.completions.algorithm = "fuzzy"
$env.config.menus ++= [{
    name: completion_menu
    only_buffer_difference: false
    marker: "| "
    type: {
        layout: columnar
        columns: 4
        col_width: 20
        col_padding: 2
    }
    style: {
        text: light_gray
        selected_text: light_gray_reverse
        description_text: gray
    }
}]

$env.config.menus ++= [{
    name: history_menu
    only_buffer_difference: true
    marker: "? "
    type: {
        layout: list
        page_size: 10
    }
    style: {
        text: light_gray
        selected_text: light_gray_reverse
        description_text: gray
    }
}]

      '';
    };
    settings = {
      show_banner = false;
      table = {
	mode = "markdown";
      };
      history = {
	file_format = "sqlite";
	max_size = 1000000;
      };
      display_errors = {
	exit_code = true;
      };
    };
  };

  programs.bash = {
    enable = true;
    historySize = -1;
    historyFileSize = -1;
    bashrcExtra = let
      dicts = [
        pkgs.hunspellDicts.de-de
        pkgs.hunspellDicts.de_DE
        pkgs.hunspellDicts.en-us
        pkgs.hunspellDicts.en_US
        pkgs.hunspellDictsChromium.de-de
        pkgs.hunspellDictsChromium.de_DE
        pkgs.hunspellDictsChromium.en-us
        pkgs.hunspellDictsChromium.en_US
      ];
      buildPath = dict: "${dict}/share/hunspell";
    in ''
      export DICPATH=${builtins.concatStringsSep ":" (builtins.map buildPath dicts)}
    '';
    initExtra = ''
      if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "nu" && -z ''${BASH_EXECUTION_STRING} ]]
      then
        shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
        exec ${pkgs.nushell}/bin/nu $LOGIN_OPTION
      fi
    '';
  };

  programs.emacs = {
    enable = true;
    #package = pkgs.emacs-git-nox;
    #package = pkgs.emacs-nox;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: [
      epkgs.vterm
      (epkgs.treesit-grammars.with-grammars (
	x: builtins.attrValues (lib.attrsets.filterAttrs (
	  n: v: if (builtins.toString n) == "tree-sitter-quint" then false else true)
	  x)))
      epkgs.use-package epkgs.powershell epkgs.edit-indirect
      epkgs.helm epkgs.treesit-auto epkgs.doom-modeline epkgs.magit
      epkgs.blacken epkgs.flycheck epkgs.yasnippet epkgs.nix-ts-mode
      epkgs.yaml-mode epkgs.yasnippet-capf epkgs.nerd-icons
      epkgs.nerd-icons-grep epkgs.smartparens epkgs.helm-rg
    ];
    extraConfig = (builtins.readFile ./init.el);
  };

  home.packages = with pkgs; [
    ispell hunspell iosevka-bin lsof file dnsutils tmux curl git tcpdump strace
    gdb ripgrep jq
  ];
}
