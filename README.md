# Wiki Mode

Wiki Mode gives you a *hypertext authoring environment* from within
Emacs. It's also a *minor mode*, which means that it can be used while
a different major mode is active.

Most of this has largely been superseded by other modes. These days
that would be [Org Mode](https://orgmode.org/).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Wiki Mode](#wiki-mode)
    - [Wiki Mode (wiki.el)](#wiki-mode-wikiel)
    - [Page Summaries (wiki-sum.el)](#page-summaries-wiki-sumel)
    - [Generating Meta Desciption Tags](#generating-meta-desciption-tags)
    - [Private Pages and Wiki Projects (wiki-projects.el)](#private-pages-and-wiki-projects-wiki-projectsel)
    - [Don't mode me](#dont-mode-me)
    - [Guess Buffer Language](#guess-buffer-language)
    - [Page Language](#page-language)
    - [Spelling](#spelling)
    - [Wiki Link (wiki-link.el)](#wiki-link-wiki-linkel)
    - [Wiki Inter (wiki-inter.el)](#wiki-inter-wiki-interel)
    - [Anchors](#anchors)
    - [Link Graphs (wiki-dot.el)](#link-graphs-wiki-dotel)
    - [Inline Images](#inline-images)
    - [Links to other parts of your own site](#links-to-other-parts-of-your-own-site)
    - [Editing a remote wiki (wiki-remote.el)](#editing-a-remote-wiki-wiki-remoteel)
        - [Setting your Username](#setting-your-username)
        - [w3m-mode](#w3m-mode)
        - [phpwiki support](#phpwiki-support)
    - [Sample Wiki Mode Setup](#sample-wiki-mode-setup)

<!-- markdown-toc end -->


## Wiki Mode (wiki.el)

Wiki Mode treats all text files in certain directories as wiki pages:

Words with mixed case are links you can follow. If a page with that
name exists, you will be taken there. If such a does not exist,
following the link will create a new page for you to fill. Links to
non-existing pages have a `?` appended so that you can see whether
following the link will give you any informatin or not (such as
`SampleUndefinedPage`). In order to follow a link, hit RET when point
is on the link, or use `mouse-2`.

This makes linking to other pages extremely easy.

All of WikiMode it is customizable, of course. You can allways change
the rules. You can allow HTML markup. You can change how the markup is
translated into HTML. You can define new markup. You can translate the
output into something other than HTML. You can change what the link
pattern.

## Page Summaries (wiki-sum.el)

The Wiki Summary feature adds a meta description tag to all HTML pages
and creates the `SiteSummary` page.

The Wiki Summary of a page is the first paragraph on the page with at
least two sentences. It's a simple trick, but it seems to work for my
pages.

Wiki Summary can be used together with Wiki Mode. Personally, I use it
for two things:

* I call `M-x wiki-write-summary` from time to time to generate the
  `SiteSummary` page for me.

* I generate meta description tags for my pages. This is more
  elaborate.

## Generating Meta Desciption Tags

Creating the meta description must happen when the page is published.
It happens in two stages. First, we determine the summary of the
current page using the function `my-wiki-store-summary` as a markup
rule. It doesn't add any markup: It just stores the page summary in
the variable `my-wiki-summary`. Later, we use the function
`my-wiki-add-summary` to insert the summary in the header.

```elisp
(load-library "wiki-sum")
(defvar my-wiki-summary nil)
(defun my-wiki-store-summary ()
  (if (member (file-name-nondirectory buffer-file-name)
              wiki-summary-exclude-pages)
      (setq my-wiki-summary nil)
    (setq my-wiki-summary (wiki-summarize))
    (when my-wiki-summary
      (while (string-match "\"" my-wiki-summary)
        (setq my-wiki-summary (replace-match "&quot;" t t my-wiki-summary))))))
(defun my-wiki-add-summary ()
  (goto-char (point-min))
  (when (and my-wiki-summary
             (search-forward "</title>" nil t))
    (insert "\n"
            "<meta name=\"description\" content=\""
            my-wiki-summary "\">")))
```

Now we need to install these two functions in `wiki-pub-rules`. See
Sample Wiki Mode Setup for an example.

You should see the meta description tag right after the `</title>`
tag.

## Private Pages and Wiki Projects (wiki-projects.el)

Should you want to use Wiki Mode for work to be published and for
private notes, you must take care not to publish your private pages.
Wiki Projects helps you do that.

This code allows you to switch between different wiki projects. Every
project is associated with a list of wiki directories, a publishing
directory and publishing rules.

Customize `wiki-projects` to set them up. The default setup includes a
public and a private project. The public project uses the `~/Wiki`
directory and publishes HTML files into the `~/WebWiki` directory. The
private project uses the `~/Wiki` and the `~/Notes` directories and it
disables publishing.

If you do not customize `wiki-projects` , the public project will take
the default values from the variables `wiki-pub-directory`,
`wiki-directories` and `wiki-pub-rules`. Once you customize
`wiki-projects`, switching to a project will overwrite any values
stored in those three variables.

As I used plain WikiMode before, what I did to define a little extra
wiki project for temporary pages from the Emacs Wiki is the following
code in my .emacs file:

```elisp
(load "wiki-projects")
(add-to-list 'wiki-projects
             `("EmacsWiki" (,(expand-file-name"~/EmacsWiki/")) nil nil))
```

## Don't mode me

Unfortunately this introduces "modes" -- if you want to treat a
certain file as a wiki page, you have to be sure to be using the
correct project. Maybe I should make publishing directory and
publishing rules buffer local variables. One unresolved issue with
that is what to do with pages that are part of several projects.

## Guess Buffer Language

This code determines the buffer language by counting several small and
often used words characteristic each language and ranks them. The
original version was posted by Jean Philippe Theberge on
news:gnu.emacs.help.

We will need this for the next two sections!

```elisp
(defvar guess-language-rules
  '(("en" . "\\<\\(of\\|the\\|and\\|or\\|how\\)\\>")
    ("de" . "\\<\\(und\\|oder\\|der\\|die\\|das\\|wie\\)\\>") 
    ("fr" . "\\<\\(et\\|ou\\|[ld]es\\|que\\)\\>")
    ("pt" . "\\<\\(de\\|para\\|e\\|ou\\|como\\)\\>"))
  "Alist of rules to determine the language of some text.
Each rule has the form (CODE . REGEXP) where CODE is a string to
identify the language (probably according to ISO 639), and REGEXP is a
regexp that matches some very common words particular to that language.
The default language should be listed first.  That will be the language
returned when no REGEXP matches, as would happen for an empty
document.")

(defun guess-buffer-language ()
  "Guess language in the current buffer."
  (save-excursion 
    (goto-char (point-min))
    (let ((count (map 'list (lambda (x)
                              (cons (string-to-number
                                     (count-matches (cdr x))) (car x)))
                      guess-language-rules)))
      (cdr (assoc (car (sort (map 'list 'car count) '>)) 
                  count)))))
```

Here is an interactive wrapper:

```elisp
(defun guess-language ()
  "Guess language in the current buffer."
  (interactive)
  (message (guess-buffer-language)))
```

On Emacs22 `count-matches` returns integer so `string-to-number`
triggers an error. Just replace this:

```elisp
(cons (string-to-number
       (count-matches (cdr x))) (car x)))
```

with this:

```elisp
(cons (count-matches (cdr x)) (car x)))
```

## Page Language

This sets the language attribute of your pages. It is an extension to
Wiki Mode and adds the LANG attribute to the BODY tag in HTML pages:
Instead of `<body>` it now says `<body lang="en">`.

Determining the buffer language must happen when the page is
published. It happens in two stages. First, we determine the language
of the current page using the function `my-wiki-store-language` as a
markup rule. It doesn't add any markup: It just stores the language in
the variable `my-wiki-language`. Later, we use the function
`my-wiki-add-language` to insert the language in the header.

This code requires a Guess Buffer Language function, see below.

```elisp
(defvar my-wiki-language nil)
(defun my-wiki-store-language ()
(setq my-wiki-language (guess-buffer-language)))
(defun my-wiki-add-language ()
  (goto-char (point-min))
  (when (and my-wiki-language
             (search-forward "<body>" nil t))
    (replace-match (format "<body lang=\"%s\">" my-wiki-language))))
```

Now we need to install this in `wiki-pub-rules`.

```elisp
(add-to-list 'wiki-pub-rules 'my-wiki-add-language)
```

## Spelling

This code sets the `ispell` dictionary automatically based on the
language returned by the Guess Buffer Language code, see above.

First, we need to match a language returned by the Guess Buffer
Language code with an `ispell` dictionary. We define a table to match
the languages with the dictionaries (note that I use a non-standard
Swiss dictionary for German texts), and we add a new funtion to the
hook for switching Wiki Mode on.

```elisp
(defvar guess-language-dictionaries
  '(("fr" . "francais")
    ("en" . "english")
    ("de" . "swiss"))
  "Alist of rules to determine the ispell dictionary for a language.
Each rule has the form (CODE . DICTIONARY) where CODE is a string to
identify the language (probably according to ISO 639), and DICTIONARY is
the name of an ispell dictionary that can be passed to
`ispell-change-dictionary'.")

(defun my-wiki-change-ispell-dictionary ()
  "Call `ispell-change-dictionary' with buffer language.
The buffer language is guessed by `guess-buffer-language'."
  (let ((language (cdr (assoc (guess-buffer-language)
                              guess-language-dictionaries))))
    (if (null language)
        (message "Language unknown, ispell dictionary unchanged")
      (message "Guessing language: %s" language)
      (ispell-change-dictionary language))))

(add-hook 'wiki-mode-on-hook 'my-wiki-change-ispell-dictionary)
```

Now we need to tell ispell that wiki names must be skipped. This is a
little bit difficult since our regexp requires case sensitive
matching. That's why I had to advise `ispell-region`. When used while
Wiki Mode is active, `case-fold-search` is set to nil. Up to now, I
haven't found any problems with that. It is a dirty hack, however.

```elisp
(defun my-wiki-ispell-skip-setup ()
  "Modify ispell to exclude wiki names.
We do that by locally setting `ispell-skip-region-alists'.
This will only work if some advice is given to ispell-region in
order to do all skipping with `case-fold-search'."
  (make-local-variable 'ispell-skip-region-alists)
  (add-to-list 'ispell-skip-region-alist (list wiki-name-regexp)))

(add-hook 'wiki-mode-on-hook 'my-wiki-ispell-skip-setup)

(defadvice ispell-region (around ispell-case-fold activate)
  "When variable `wiki-mode' is non-nil, `case-fold-search' will be
bound to nil."
  (let ((case-fold-search case-fold-search))
    (when wiki-mode
      (setq case-fold-search nil))
    ad-do-it))
```

## Wiki Link (wiki-link.el)

This allows you to use an extended link pattern for WikiMode. The
extended link pattern uses extra square brackets.

Examples: 

```text
Extended Link           Resulting HTML
\-----------------------------------------------------
[[LINK]]                <a href="LINK">LINK</a>
[[LINK][NAME]]          <a href="LINK">NAME</a>
```

In order to do this, you must `M-x customize-option` the list of
formatting rules, `wiki-pub-rules`. Add the following two rules at the
end:

```text
INS DEL Choice: Value Menu Rule:
            Choice: Value Menu Search a regexp: \[\[\([^]]+\)\]\[\([^]]+\)\]\]
            Choice: Value Menu Insert or replace a string: <a href="\1">\2</a>
INS DEL Choice: Value Menu Rule:
            Choice: Value Menu Search a regexp: \[\[\([^]]+\)\]\]
            Choice: Value Menu Insert or replace a string: <a href="\1">\1</a>
```

This doesn't change `wiki-name-regexp` and thus `wiki-next-reference`
and friends will not work.

## Wiki Inter (wiki-inter.el)

This extension to WikiMode allows you to use a prefix notation for a
known number of hosts on the web. Therefore, instead of having to type
the entire URL to link to a particular page on one of these sites, you
can use the shorthand notation HOST:PAGE.

If you want to add another host to the list list of recognized hosts,
customize `wiki-inter-links`.

Note that in order to publish, `wiki-inter-link-publish` must be on
`wiki-pub-rules`.  Loading the file puts it there, but if you
customized the rules or set them manually, you will have to add
`wiki-inter-link-publish` to `wiki-pub-rules` yourself.  See
Sample Wiki Mode Setup for an example.

The simplest way to install it is adding `(require 'wiki-inter)` after
`(require 'wiki)`.

## Anchors

This allows you to use anchors with WikiMode. To make an anchor, put
"#Anchor" at the beginning of the line you want to link to. To link to
an anchor, use the `PageName#Anchor` link pattern.

Examples:

```text
Anchor                Resulting HTML
\-----------------------------------------------------
#NAME                   <a name="NAME">
PAGE#NAME               <a href="PAGE#NAME">PAGE</a>
```

Here are the necessary rules to it. Customize `wiki-pub-rules` and add
these rules *after* `wiki-replace-links`.

The rule for the `#ANCHOR`:

```text
Regexp:      ^#\(\sw+\)
Replacement: <a name="\1">
```

The rule for <nowiki>PageName#Anchor</nowiki> references:

```text
Regexp:      <a href="\(\(\|.\)*\)\.html">\(\(\|.\)*\)</a>#\(\sw+\)
Replacement: <a href="\1.html#\5">\3</a>
```

Note how the second rule just rearranges the HTML links produced by
`wiki-replace-links`.

If you want more elaborate control over the output, you will have to
combine this with the Wiki Link code. But then again, if you might be
better off with a different wiki mode.

## Link Graphs (wiki-dot.el)

This allows you to parse all wiki files and generate a dot file. The
dot file can then be transformed into graphs by other tools.

The graph is described using the dot language. See
http://www.graphviz.org/ for source and binary distributions. There
are two tools available: `dot` for directed graphs and `neato` for
undirected graphs. Customize `wiki-graph-type` to control the output.

Use `M-x wiki-graph` to generate a dot file for you. You will need a
working copy of `neato` or `dot` to process the file.

I've found it very difficult to produce acceptable results with neato
and dot. It is rather difficult to determine which pages ought shall
be part of the graph and which pages shall not. Even you find an
appropriate `wiki-include-function`, you will have to fiddle with dot
or neato parameters to produce acceptable output. If you find good
wiki-include-functions or dot/neato parameters to generate interesting
graphs of your wiki, please share them with me. I'm allways interested
in learning more! 😊 -- AlexSchroeder

## Inline Images

Define an appropriate rule for `wiki-pub-rules`. Here is an example
rule:

```elisp
    Regexp:      \<Inline:\(\(\sw\|[-_.]\)+\)\.\(png\|jpg\)\>
    Replacement: <a href="pics/\1.\3"><img src="pics/\1.\3" alt="\1.\3"></a>
```

That would replace the following:

```text
Inline:somename.png
```

with this:

```html
<a href="pics/somename.png"><img src="pics/somename.png" alt="somename.png"></a>
```

In effect, it would be replaced with the `<img src="...">` tag, marked
as a link and using an appropriate alt text, assuming that all
pictures reside in the pics subdirectory.

## Links to other parts of your own site

The easiest way to do this is using Wiki Inter code. Just add virtual
hosts to `wiki-inter-links`. Here is the complete list of virtual
hosts I'm using:

```text
Interwiki Host: Main
URL Fragment: http://www.geocities.com/kensanata/%s.html

Interwiki Host: Code
URL Fragment: http://www.geocities.com/kensanata/elisp/%s.txt

Interwiki Host: Dir
URL Fragment: http://www.geocities.com/kensanata/%s/

Interwiki Host: Pic
URL Fragment: http://www.geocities.com/kensanata/%s
```

You could have achieved a similar effect using a rule in
`wiki-pub-rules`. Such rules would not affect highlighting, however.

## Editing a remote wiki (wiki-remote.el)

(Note that these days there are better solutions! Simple Wiki Edit
Mode replaces WikiRemote and no longer depends on url.el, for
example.)

This package allows you to load and save pages from a remote wiki
using w3 functionality. Just call `wiki-remote-get` and enter page
name and wiki name. In order to save, hit `C-c C-c`.

`wiki-remote.el` requires `w3` or `w3m`.

The state of support for various wiki engines is unclear. It used to
work for Usemod, Zwiki and CLiki. As Oddmuse inherited the Usemod API,
it should also work.

If you want to follow links, you need a wiki mode to edit the file
itself -- either Wiki Mode (see above) or Emacs Wiki Mode. The default
is plain Text Mode, which is good enough to edit the page.

There is a customizable option that allows you to choose which
function to call once wiki-remote.el has downloaded the remote page.
The new buffer will be in `wiki-remote-mode`, a minor mode that has
just one keybinding: `C-c C-c`. That will save the page back to the
server.

Usemod detects editing conflicts by adding hidden fields to the form,
so this data has to be stored in buffer local variables.  Zwiki
detects editing conflicts using timestamps, I think.  When I tested
this 2001-05-25, however, I saw no such timestamps while editing zwiki
pages.  Who knows, maybe this has been rendered obsolete?

Anyway, this thing is *useful for looong pages* -- because Netscape
4.74 for GNU/Linux (the one I am using) will crash when you are
editing very long pages (20k and more). Emacs to the rescue! Get the
page using `wiki-remote.el`, edit, save, and continue browsing using
your favorite browser...

### Setting your Username

If you are using `wiki-remote-get` to edit a page, you can save your
edits using `C-c C-c`. All operations will use `url-retrieve` which is
part of the `w3` package.

Surf to the Emacs Wiki using `w3` and set your Preferences as usual.
Note that you must delete the password. When I tried it, the default
value of "*" caused an error when I saved my changes. Once you do
that, the cookie will be created. Don't forget to save the cookie! Use
`M-: (url-cookie-write-file)` to do that.

### w3m-mode

Yes, if you have `emacs-w3m`, you can use it as well. Here is from a
mail, slightly edited.

```
 From: KahlilHodgson
 Subject: wiki-remote.el
 To: AlexSchroeder
 Date: Fri, 27 Sep 2002 08:58:29 +1000
```

Here's some code that makes wiki-remote.el work if you have emacs-w3m
installed INSTEAD of w3.

```elisp
(unless (condition-case nil
			(require 'w3)
		  (error nil))
  (require 'w3m)

  (defvar url-request-method nil)
  (defvar url-request-data nil)
  (defvar url-inhibit-mime-parsing nil)
  (defalias 'url-hexify-string 'w3m-url-encode-string)

  (defun url-retrieve (url)
	"Return a buffer containing the HTML contents of URL using w3m."
	(let ((w3m-async-exec nil)) ;; wait for output
	  (set-buffer (get-buffer-create " *wiki-edit*")) ;; invisible
	  (erase-buffer) ;; flush previous edit
	  (w3m-retrieve url nil 'no-cache url-request-data)
	  ;; want the cdr of the return value to a buffer object
	  `(t . ,(current-buffer))))
  )
```

### phpwiki support

```elisp
;; PhpWiki 1.3 support by ReiniUrban <rurban@x-ray.at>

(defun phpwiki-get-page (page url)
  "Return a buffer with the plain text contents of PAGE at URL."
  (let* ((data (phpwiki-get-data page url))
	 (text (cdr (assq 'text data)))
	 (buf (get-buffer-create page)))
	(set-buffer buf)
	(erase-buffer)
	(insert text)
	;(setq phpwiki-data data)
	buf))

(defun phpwiki-get-data (page url)
  "Extract the textarea content from the page."
  (message "Reading %s at %s" page url)
  (save-excursion
	(let ((buf (cdr (url-retrieve (concat url "?action=viewsource&pagename=" 
					  (url-hexify-string page))))))
	  (when (null buf)
	(error "Could not retrieve %s" url))
	  (when wiki-remote-debug
	(switch-to-buffer-other-window buf))
	  (set-buffer buf)
	  (goto-char (point-min))
	  (let ((text 
		 (if (search-forward-regexp "<textarea class=\"wikiedit\"\nname=\"content\"")
		 (let ((start (search-forward-regexp ">"))
			   end)
		   (search-forward "</textarea>")
		   (setq end (copy-marker (match-beginning 0)))
		   (mapcar (lambda (pair)
				 (wiki-remote-massage (car pair) (cdr pair) start end))
			   '(("&gt;" . ">")
				 ("&lt;" . "<")
				 ("&quot;" . "\"")
				 ("&amp;" . "&")
				 ("^M" . "")))
		   (buffer-substring start end)))))
	`((text . ,text)
	  (title . ,page))))))


(defun phpwiki-put-page (page url content)
  "Save buffer to wiki at URL with new CONTENT."
  (interactive)
  (unless (buffer-modified-p)
	(error "No changes need to be saved"))
  (let ((url-request-method "POST")
	(url-request-data
	 (concat
	  "edit[save]=Save&edit[summary]=" (url-hexify-string (read-from-minibuffer "Summary: " ""))
	  "&action=edit&pagename=" (url-hexify-string page)
	  "&edit[content]=" (url-hexify-string content)))
	;:(url-inhibit-mime-parsing t) ; ??
	(request (concat url (url-hexify-string page) "?action=edit")))
	(message "Saving...")
	(let ((buf (cdr (url-retrieve request))))
	  (message "Verifying...")
	  (when wiki-remote-debug
	(switch-to-buffer-other-window buf))
	  (when (not (phpwiki-verify-page buf page))
	(error "Error saving page")))))


(defun phpwiki-verify-page (buf page)
  "Verify that buffer BUF actually contains Saved: PAGE.
This should catch all errors caused by the wiki engine
that do not result in a HTTP error."
  (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(and (search-forward "<!-- Page title -->" nil t)
	 ;; Strip out localized "<h1>Saved: " message
	 ;; An unsuccessful save has <h1><a href="HomePage?action=BackLinks"
	 (search-forward (concat "<a href=\"" page "\" class=\"wiki\">" 
				 page 
				 "</a></h1>\n<!-- End top -->") nil t))))
```

## Sample Wiki Mode Setup

The setup I use with Wiki Mode works in Emacs and XEmacs.

I use text-mode as my default major mode, therefore all wiki pages are
in text mode. In text mode, I want the apostrophe to be considered a
word separator.

```elisp
(setq default-major-mode 'text-mode)
(modify-syntax-entry ?' "." text-mode-syntax-table)
```

I want to use two spaces after a period. If I use one space, I want to
alert myself to that. And when I use the magic `FIXME` string in text
files, I want it to stand out.

```elisp
(defface extra-whitespace-face
  '((t (:background "pale green")))
  "Used in text-mode and friends for exactly one space after a period.")
(font-lock-add-keywords
  'text-mode
  '(("FIXME[:!]?" 0 'show-paren-mismatch-face)
    ("\\.\\( \\)\\b" 1 'extra-whitespace-face)))
```

Now for XEmacs, we need to load `easy-mmode.el`. I just use the source
file from my Emacs lisp directory. And we need some time stuff for
wiki interlinks, therefore I load these files from my Gnus directory.
You might have them stored in other directories, obviously.

```elisp
(when (featurep 'xemacs)
  (load-file "/usr/local/share/emacs/20.7/lisp/emacs-lisp/easy-mmode.el")
  (load "~/elisp/gnus/lisp/parse-time.el" t t t)
  (load "~/elisp/gnus/lisp/time-date.el" t t t))
```

We are getting to the wiki stuff at last. Load the mode and switch on
`font-lock` and `auto-fill`.

```elisp
(load-library "wiki")
(load-library "wiki-inter")
;; Usually text-mode buffers don't use font-lock!
(add-hook 'wiki-mode-on-hook 'turn-on-font-lock)
(add-hook 'wiki-mode-on-hook 'turn-on-auto-fill)
```

In order to navigate and fill bullet lists, I patched my `fill.el`
(thanks to Stefan Monnier). A marginal improvement.

```elisp
(load-library "fill"); Stefan's bugfix included
(add-hook 'wiki-mode-on-hook (lambda ()
                               (setq paragraph-start "\\*\\|$" 
                                     paragraph-separate "$")))
```

I want to use Shift TAB on GNU/Linux running under X to jump to the
previous reference.

```elisp
(if (not (featurep 'xemacs))
    (define-key wiki-mode-map '[(shift iso-lefttab)] 'wiki-previous-reference)
  (define-key wiki-mode-map '[(iso-left-tab)] 'wiki-previous-reference))
```

And now for the big thing. The markup. I added Wiki Summary and Wiki
Language functions as well as the quoting of the ampersand (`&`).
Other customized variables such as directories, maintainer mail
address, and index page I keep to myself. 😎

These are the new XHTML 1.0 publishing rules:

```elisp
(setq wiki-pub-rules
 `(my-wiki-store-language
   my-wiki-store-summary
   ("&" . "&amp;")
   ("<" . "&lt;")
   (">" . "&gt;")
   ("&#39;&#39;\\(\\(\\|.\\)*\\)&#39;&#39;" . "<strong>\\1</strong>")
   ("\\`\n*" . "<p>\n"); remove emty lines and add <p> at the beginning
   ("\n+\\'" . ""); remove emty lines at the end
   (end-of-buffer . "\n</p>"); add </p> at the end
   ("\n\n+" . "\n</p>\n<p>\n"); insert </p><p> between all paragraphs
   ("^\\*[  ]*" . "</li>\n<li>")
   ("\n</li>" . "</li>")
   ("<p></li>\\(\\(\\|.\\|\n\\)+\\)\n</p>" . "<ul>\\1</li>\n</ul>")
   ("<p>\n\\([  ]+\\(\\|.\\|\n\\)+\\)</p>" . "<pre>\n\\1</pre>")
   ("<p>\n:\\(\\(\\|.\\|\n\\)+\\)</p>" . "<blockquote>\n<p>\n \\1</p>\n</blockquote>")
   ;; ("<p>\n\\'" . "")
   ,(cons thing-at-point-url-regexp "<a href=\"\\&\">\\&</a>")
   ("[-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+" . "<a href=\"mailto:''''\\&\">\\&</a>")
   wiki-replace-links
   wiki-inter-link-publish
   (beginning-of-buffer . "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"DTD/xhtml1-strict.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n<title><?name></title>\n<link rel=\"contents\" href=\"../index.html\" title=\"Table of Contents\"/>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8559-1\"/>\n<meta http-equiv=\"Content-Style-Type\" content=\"text/css\"/>\n<link rel=\"stylesheet\" type=\"text/css\" href=\"../wiki.css\"/>\n</head>\n<body>\n<p>\n<a href=\"SiteMap.html\">SiteMap</a> /\n<a href=\"AllPages.html\">AllPages</a> /\n<a href=\"../index.html\">Out</a>\n</p>\n<h1><a name=\"<?name>\" title=\"<?name>\"><?name></a></h1>\n")
   ("<\\?name>" . wiki-page-name)
   (end-of-buffer . "\n<hr></hr>\n<p>\n<a href=\"SiteMap.html\">SiteMap</a> /\n<a href=\"AllPages.html\">AllPages</a> /\n<a href=\"../index.html\">Out</a> /\n<a href=\"mailto:kensanata@''''yahoo.com\">kensanata@yahoo.com</a> /\nLast change: <?date>\n</p>\n</body>\n</html>")
   ("<\\?date>" . wiki-current-date)
   my-wiki-add-language
   my-wiki-add-summary))
```

This is the old HTML 3.2 markup:

```elisp
(setq wiki-pub-rules
 `(my-wiki-store-language
   my-wiki-store-summary
   ("&" . "&amp;")
   ("<" . "&lt;")
   (">" . "&gt;")
   ("&#39;&#39;\\(\\(\\|.\\)*\\)&#39;&#39;" . "<strong>\\1</strong>")
   ("\\`\n*" . "<p>\n")
   ("\n\n+" . "\n\n<p>\n")
   ("^\\*[  ]*" . "<li>")
   ("<p>\n<li>\\(\\([^\n]\n?\\)+\\)" . "<p>\n<ul>\n<li>\\1</ul>\n")
   ("<p>\n\\([  ]+\\([^\n]\n?\\)+\\)" . "<p>\n<pre>\n\\1</pre>\n")
   ("<p>\n:\\(\\([^\n]\n?\\)+\\)" . "<blockquote>\n<p>\n\\1</blockquote>\n")
   ,(cons thing-at-point-url-regexp "<a href=\"\\&\">\\&</a>")
   ,(cons goto-address-mail-regexp "<a href=\"mailto:\\&\">\\&</a>")
   wiki-replace-links
   wiki-inter-link-publish
   (beginning-of-buffer . "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">\n<html>\n<head>\n<title><?name></title>\n<link rel=\"contents\" href=\"../index.html\" title=\"Table of Contents\">\n<link rel=\"index\" href=\"../site-index.html\" title=\"Site Index\">\n<meta http-equiv=\"Content-Type\&quot; content=\"text/html; charset=iso-8559-1\">\n<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">\n<link rel=stylesheet type=\"text/css\" href=\"../wiki.css\">\n</head>\n<body>\n<p>\n<a href=\"SiteMap.html\">SiteMap</a> /\n<a href=\"AllPages.html\">AllPages</a> /\n<a href=\"../index.html\">Out</a>\n<h1><a name=\"<?name>\" title=\"<?name>\"><?name></a></h1>\n")
   ("<\\?name>" . wiki-page-name)
   (end-of-buffer . "\n<hr>\n<p>\n<a href=\"SiteMap.html\">SiteMap</a> /\n<a href=\"AllPages.html\">AllPages</a> /\n<a href=\"../index.html\">Out</a> /\n<a href=\"mailto:kensanata@yahoo.com\">kensanata@''''yahoo.com</a> /\nLast change: <?date>\n</body>\n</html>")
   ("<\\?date>" . wiki-current-date)
   my-wiki-add-language
   my-wiki-add-summary))
```

Another variant is to keep `wiki-pub-rules` defaults, and only change
selected entries. Here, for example, I set the `end-of-buffer` rule
only:

```elisp
    (defun href (page &optional link)
      (setq link (or link page))
      (concat "<a href=\"" link ".html\">" page "</a>"))

    (defun mailto (address)
      (concat "<a href=\"mailto:" address "\">" address "</a>"))

    (setcdr (assq 'end-of-buffer wiki-pub-rules)
            (concat "<hr>\n<p>\n"
                    (href "SiteMap") " / "
                    (href "AllPages" "index") " / "
                    (href "Out" "../index") " / "
                    (mailto "alex@emacswiki.org") " / "
                    "Last change: <?date>\n"
                    "</body>\n"
                    "</html>"))
```

If you want to add Wiki Language support, then all you really need is
this simple `add-to-list`:

```elisp
(add-to-list 'wiki-pub-rules 'my-wiki-add-language)
```
