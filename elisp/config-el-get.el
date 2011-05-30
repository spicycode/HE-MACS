(setq el-get-sources
      '(ack ruby-mode el-expectations clojure-mode color-theme haml-mode flymake-ruby slime highlight-symbol
	(:name smex
	       :type git
	       :url "https://github.com/nonsequitur/smex.git")
	(:name javascript
	       :type elpa)
	(:name magit
	       :type git
	       :url "https://github.com/philjackson/magit.git")
	(:name cucumber.el
	       :type git
	       :url "https://github.com/michaelklishin/cucumber.el.git")
	(:name rvm.el
	       :type git
	       :url "https://github.com/senny/rvm.el.git")
	(:name rspec-mode
	       :type git
	       :url "https://github.com/aredington/rspec-mode.git")
	(:name ruby-electric
	       :type elpa)
	(:name slime-repl
	       :type elpa)
	(:name paredit
	       :after (lambda () (add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)))
	(:name color-theme-ir-black 
	       :type git 
	       :url "git://github.com/burke/color-theme-ir-black.git")
	(:name textmate.el
	       :type git
	       :url "https://github.com/defunkt/textmate.el.git")
	(:name autocomplete
	       :type git
	       :url "https://github.com/m2ym/auto-complete.git")
	))
(el-get 'sync)
