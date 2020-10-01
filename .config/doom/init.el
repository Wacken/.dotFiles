;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;japanese

       :completion
       (company +tng)                   ; the ultimate code completion backend
       (ivy                ;; +fuzzy
        +prescient +icons) ; a search engine for love and life

       :ui
       ;; deft                     ; notational velocity for emacs
       doom                             ; what makes doom look the way it does
       doom-dashboard                   ; a nifty splash screen for emacs
       ;; doom-quit                ; doom quit-message prompts when you quit emacs
       fill-column            ; a `fill-column' indicator
       hl-todo                ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;; indent-guides            ; highlighted indent columns
       (ligatures +fira +extra) ; ligatures and symbols to make your code pretty again
       minimap                  ; show a map of the code on the side
       modeline                 ; snazzy, atom-inspired modeline, plus api
       nav-flash                ; blink cursor line after big motions
       ;; neotree                  ; a project drawer, like nerdtree for vim
       ophints                 ; highlight the region an operation acts on
       (popup +defaults)       ; tame sudden yet inevitable temporary windows
       ;; tabs                     ; a tab bar for emacs
       treemacs                      ; a project drawer, like neotree but cooler
       ;; unicode                  ; extended unicode support for various languages
       vc-gutter              ; vcs diff in the fringe
       vi-tilde-fringe        ; fringe tildes to mark beyond eob
       window-select          ; visually switch windows
       workspaces             ; tab emulation, persistence & separate workspaces
       zen                    ; distraction-free coding or writing

       :editor
       (evil +everywhere)   ; come to the dark side, we have cookies
       file-templates       ; auto-snippets for empty files
       fold                 ; (nigh) universal code folding
       (format +onsave)     ; automated prettiness
       ;; lispy                ; vim for lisp, for people who don't like vim
       multiple-cursors     ; editing in many places at once
       parinfer          ; turn lisp into python, sort of
       rotate-text               ; cycle region at point between text candidates
       snippets                  ; my elves. they type so i don't have to
       ;; word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)  ; interactive buffer management
       (undo +tree)      ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and emacs, sitting in a tree

       :term
       eshell         ; the elisp shell that works everywhere
       ;;shell             ; simple shell repl for emacs
       ;;term              ; basic terminal emulator for emacs
       ;;vterm             ; the best terminal emulation in emacs

       :checkers
       (syntax +childframe)           ; tasing you for every semicolon you forget
       (spell +everywhere +hunspell)             ; tasing you for misspelling mispelling fs
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       biblio
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup +dictionary +offline)              ; navigate your code and its documentation
       (lsp +peek)
       magit                    ; a git porcelain for emacs
       make              ; run make tasks from emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an api for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       ;; (:if is-mac macos)  ; improve compatibility with macos
       ;;tty               ; improve the terminal emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;cc                ; c/c++/obj-c madness
       (clojure)           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       (csharp +unity)            ; unity, .net, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of tea?
       emacs-lisp                       ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ml stands for microsoft's language
       ;;fstar             ; (dependent) types and (monadic) effects and z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       (haskell +dante)  ; a language that's lazier than i am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;json              ; at least it ain't xml
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster matlab
       ;;kotlin            ; a better, slicker java(script)
       ;;latex             ; writing papers in emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in emacs
       ;;lua               ; one-based indices? one-based indices
       ;; markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; i hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       org              ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a dsl for dsls
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; emacs as a rest client
       ;;rst               ; rest in peace
       ;;(ruby +rails)     ; 1.step {|i| p "ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; fe2o3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the c xor
       ;;sml
       ;;solidity          ; do you need a blockchain? no.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; earth and moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; json, but readable

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       ;;irc               ; how neckbeards socialize
       (rss +org)                       ; emacs as an rss reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens))
