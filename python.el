(add-hook 'python-mode-hook
	  (lambda ()
	    (fci-mode)
	    (setq fill-column 80)))
