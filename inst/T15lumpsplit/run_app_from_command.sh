## Attempt to run the app from a command.
## Not successful so far.

# BTW,
# browseURL('http://localhost/~Roger/Rep.pdf')
#   works, but
# browseURL('http://localhost/~Roger/T15lumpsplit/Reproducibility-lump-split-page-1.pdf')
#   doesnt.

Bias,variance,smoothing,shrinking.Rmd

/usr/local/bin/pandoc +RTS -K512m -RTS \
/private/var/folders/6v/y_cp6z4d22ggbgrqdk0g73v80000gv/T/RtmpOJU6le/Bias,variance,smoothing,shrinking.utf8.md\
--to html4 \
--from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash+smart \
--output /private/var/folders/6v/y_cp6z4d22ggbgrqdk0g73v80000gv/T/RtmpOJU6le/file1409f7c6c4fc4.html \
--email-obfuscation none --standalone --section-divs \
--table-of-contents --toc-depth 6 --variable toc_float=1 \
--variable toc_selectors=h1,h2,h3,h4,h5,h6 \
--variable toc_collapsed=1 --variable toc_print=1 \
--template /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rmarkdown/rmd/h/default.html\
--no-highlight --variable highlightjs=1 --number-sections \
--css lumpsplit.css --id-prefix section-
--variable 'theme:bootstrap' --mathjax \
--variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'

# Output created: /private/var/folders/6v/y_cp6z4d22ggbgrqdk0g73v80000gv/T/RtmpoiuOwg/file18a5220af7df.html

browseURL("http://127.0.0.1:7647/")


