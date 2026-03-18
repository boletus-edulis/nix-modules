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
            binary: '#ba8baf'
            block: '#7cafc2'
            cell-path: '#d8d8d8'
            closure: '#86c1b9'
            custom: '#f8f8f8'
            duration: '#f7ca88'
            float: '#ab4642'
            glob: '#f8f8f8'
            int: '#ba8baf'
            list: '#86c1b9'
            nothing: '#ab4642'
            range: '#f7ca88'
            record: '#86c1b9'
            string: '#a1b56c'

            bool: {|| if $in { '#86c1b9' } else { '#f7ca88' } }

            datetime: {|| (date now) - $in |
              if $in < 1hr {
                { fg: '#ab4642' attr: 'b' }
              } else if $in < 6hr {
                '#ab4642'
              } else if $in < 1day {
                '#f7ca88'
              } else if $in < 3day {
                '#a1b56c'
              } else if $in < 1wk {
                { fg: '#a1b56c' attr: 'b' }
              } else if $in < 6wk {
                '#86c1b9'
              } else if $in < 52wk {
                '#7cafc2'
              } else { 'dark_gray' }
            }

            filesize: {|e|
              if $e == 0b {
                '#d8d8d8'
              } else if $e < 1mb {
                '#86c1b9'
              } else {{ fg: '#7cafc2' }}
            }

            shape_and: { fg: '#ba8baf' attr: 'b' }
            shape_binary: { fg: '#ba8baf' attr: 'b' }
            shape_block: { fg: '#7cafc2' attr: 'b' }
            shape_bool: '#86c1b9'
            shape_closure: { fg: '#86c1b9' attr: 'b' }
            shape_custom: '#a1b56c'
            shape_datetime: { fg: '#86c1b9' attr: 'b' }
            shape_directory: '#86c1b9'
            shape_external: '#86c1b9'
            shape_external_resolved: '#86c1b9'
            shape_externalarg: { fg: '#a1b56c' attr: 'b' }
            shape_filepath: '#86c1b9'
            shape_flag: { fg: '#7cafc2' attr: 'b' }
            shape_float: { fg: '#ab4642' attr: 'b' }
            shape_garbage: { fg: '#FFFFFF' bg: '#FF0000' attr: 'b' }
            shape_glob_interpolation: { fg: '#86c1b9' attr: 'b' }
            shape_globpattern: { fg: '#86c1b9' attr: 'b' }
            shape_int: { fg: '#ba8baf' attr: 'b' }
            shape_internalcall: { fg: '#86c1b9' attr: 'b' }
            shape_keyword: { fg: '#ba8baf' attr: 'b' }
            shape_list: { fg: '#86c1b9' attr: 'b' }
            shape_literal: '#7cafc2'
            shape_match_pattern: '#a1b56c'
            shape_matching_brackets: { attr: 'u' }
            shape_nothing: '#ab4642'
            shape_operator: '#f7ca88'
            shape_or: { fg: '#ba8baf' attr: 'b' }
            shape_pipe: { fg: '#ba8baf' attr: 'b' }
            shape_range: { fg: '#f7ca88' attr: 'b' }
            shape_raw_string: { fg: '#f8f8f8' attr: 'b' }
            shape_record: { fg: '#86c1b9' attr: 'b' }
            shape_redirection: { fg: '#ba8baf' attr: 'b' }
            shape_signature: { fg: '#a1b56c' attr: 'b' }
            shape_string: '#a1b56c'
            shape_string_interpolation: { fg: '#86c1b9' attr: 'b' }
            shape_table: { fg: '#7cafc2' attr: 'b' }
            shape_vardecl: { fg: '#7cafc2' attr: 'u' }
            shape_variable: '#ba8baf'

            foreground: '#d8d8d8'
            background: '#181818'
            cursor: '#d8d8d8'

            empty: '#7cafc2'
            header: { fg: '#d8d8d8' attr: 'b' }
            hints: '#585858'
            leading_trailing_space_bg: { attr: 'n' }
            row_index: { fg: '#d8d8d8' attr: 'b' }
            search_result: { fg: '#ab4642' bg: '#d8d8d8' }
            separator: '#d8d8d8'
          }
        }

        $env.config.color_config = (color_theme)
      '';
    };
    settings = {
      show_banner = false;
      table = {
	mode = "basic";
      };
      history = {
	file_format = "sqlite";
	max_size = 1000000;
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
      epkgs.use-package epkgs.nushell-mode epkgs.powershell
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
