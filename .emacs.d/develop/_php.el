(add-hook 'php-mode-hook 'smartparens-mode)

(load "~/.emacs.d/packages/php-beautifier/php-beautifier")
(setq php-beautifier-executable-path "/opt/local/lib/php/pear/bin/php_beautifier")
