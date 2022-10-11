# message-links.el

Emacs library to add reference links in message-mode without html.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [What it does](#what-it-does)
- [How to use](#how-to-use)
- [customization](#customization)
- [FAQ](#faq)
<!-- markdown-toc end -->


## What it does

Consider the following textual message with a long link:

``` text
Hello,

blablabla ...
... and the report is available in the following link: https://my-file-server.org/Y5yu9Z1+3K83NFyPRy0siDw583mc+4iEljFmkUdgIIJuE5OAZTyhV00rXSMu/LP6z/o8HoD8hnXVIg2bwkddXw==
Blablabla ...

Cheers

```

This could be transformed into:

``` text
Hello,

blablabla ...
... and the report is available in the link [1]
Blablabla ...

Cheers

---links---
[1] : https://my-file-server.org/Y5yu9Z1+3K83NFyPRy0siDw583mc+4iEljFmkUdgIIJuE5OAZTyhV00rXSMu/LP6z/o8HoD8hnXVIg2bwkddXw==
```

This mode allow the generation of the references in the text as follow:
![message-links demo](doc/message-links-demo.gif)

## How to use

- Download this repository into `~/.emacs.d`
``` elisp
(load "~/.emacs.d/message-links.el/message-links.el")
(define-key message-mode-map (kbd "C-c l") 'message-links-add)
```

Then press `C-c l` when composing a message to add a link.

## Commands

- `message-links-add-link` enter a new link.
- `message-links-convert-link-at-point` match a link at the cursor location, and convert it to a referenced link
  (this uses `message-links-match-link-at-point-fn` to identify links).
- `message-links-convert-links-all` convert all links to references in the buffer or active region
  (using `message-links-match-link-forward-fn` to scan for links).
- `message-links-renumber-all` re-number all links starting from `message-links-index-start`,
  putting the footnotes in order if necessary. This can be useful when editing paragraphs that contain links which may
  become un-ordered.

## customization

- `message-links-link-header` : Default = `---links---` :<br>
  Header use to separate links and the original text. If set to `nil`, disable the header.
- `message-links-index-start` : Default = `1` :<br>
  Start index of links. So by default the first link will be `[1]`.
- `message-links-sep-footnotes-link` : Default = `'("[" . "] : " ` :<br>
  The text to use for links in the bottom of the buffer. Default, links look like `[1] : link text`.
  Customize with `(setq message-links-sep-footnotes-link '("{^" . "}: "))`
  and links in footnote will look like `{^1}: link text`
- `message-links-sep-text-link` : Default = `'("[" . "]")` :<br>
  The text to use for links in the text. Default, links look like `blablabla [1] blablabla`.
  Customize with `(setq message-links-sep-text-link '("{^" . "}"))` and links in text
  will look like `blablabla {^1} blablabla`
- `message-links-match-link-at-point-fn` : Defaults to using thing-at-pt (url).<br>
  Return the bounds of the link at the point as a cons cell or nil.
- `message-links-match-link-forward-fn` : Defaults to
  stepping over white-space for the next `message-links-match-link-at-point-fn`.<br>
  Takes a single limit argument (representing a buffer position not to seek past),
  returns the bounds of the next link or nil when none are found.
- `message-links-limit-range-fn` : Defaults to returning `((point-min) . (point-max))`.<br>
  Use this to limit the range used for scanning and link insertion.
  Useful when writing commit messages which often show commented status text at the buffer end.
  Often it's preferable to add the links before these comments. For example, see ([FAQ](#faq)).


# FAQ

## I don't want to put the links at the bottom of the buffer

  If you work in a GIT/SVN message commit or a message with a lot of reply,
  you want your links at the bottom of your message and not the bottom of the buffer.
  Use `message-links-limit-range-fn` for this purpose:
  By default:
  ``` text
  Hello,
  
  Blabla [1], babla [2].
  
  > On Thu, 1 Sep 2022, Toto wrote:
  > blablabla
  > bla
  
  ---links---
  [1] : foo.org
  [2] : bar.org
  ```
  
  In fact, wou want to add links just before the repy by tweaking `message-links-limit-range-fn`:  
  
  ``` elisp
  (setq message-links-limit-range-fn (lambda ()
             (let ((min (point-min))(max (point-max)))
               (save-excursion
                 (goto-char min)
                 (forward-line 1)
                 (setq min (point))
                 (when (re-search-forward
                        (concat "^>")
                        nil t)
                   (setq max (line-beginning-position))))
               (cons min max))))
  ```
  
  It produces
  
  ``` text
  Hello,
  
  Blabla [1], babla [2].
  
  ---links---
  [1] : foo.org
  [2] : bar.org
  
  > On Thu, 1 Sep 2022, Toto wrote:
  > blablabla
  > bla
  
  ```
  
