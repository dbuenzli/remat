# DOING
* Parse Index_d, Doc_d.

# www

* Keyboard shortcuts
* UI state
** f - toggle full screen
** s - save location viewport => #identify

* Panning
** Mouse click or tap, hold and move on image.
** Trackpad gesture. 
** Keyboard <- -> ^ v 
** http://andywoodruff.com/blog/map-panning-and-zooming-methods

* Zooming
** Double click/tap.
** Pinch gesture
** Keyboard 1,2,3,4,5 - 100%,200%,300,..
** Keyboard 0 - Fit to page
** Keyboard w - Fit to width
** Keyboard z - toggle fit/100% 

* Previous/Next page
** Keyboard n - next page
** Keyboard p - previous page

** clic/tap on the right of the image
** page down/page up

* Search on page
* Help 
** Help bubbles, see e.g. http://css-infos.net/property/-webkit-transition

* Magnifier
* Zoom 
* Minimal supported browser versions
** FF8
** IE9
** Safari 5.0

* Design Goals
** Respect browser back/forward 
** Sensible browser history
** Keyboard support for advanced users
** SHOW THE DATA, SHOW THE DATA, SHOW THE DATA

* Image scaling FF ubuntu https://bugzilla.mozilla.org/show_bug.cgi?id=486918
* http://stackoverflow.com/questions/90178/make-a-div-fill-the-remaining-screen-space

# luigi

* TODO --no-ui option
* remove ui ? 
* TODO documentation add examples. 
* TODO convert finereader -> finereader6
* TODO convert bpin -> ptext 
* TODO publish, check missing locales in data
* Cross-page hyphenation,
  La voix du Peuple_1906_01_27_f_0001.tif
  http://gallica.bnf.fr/ark:/12148/bpt6k2627644.r=langFR

* Language ID, n-grams.
** http://alias-i.com/lingpipe/demos/tutorial/langid/read-me.html
** http://corpora.uni-leipzig.de/

* Vowels
** French vowels aeiouyâàëéêèïîôûù
** Italian vowels aeiouàèìòù	

* Mailing list
** http://www.freelists.org/
** http://librelist.com/
** http://mlmmj.org/

* external indexing
http://support.google.com/webmasters/bin/answer.py?hl=en&answer=174992
https://github.com/tmpvar/jsdom
http://www.phantomjs.org/
** Provide support for web search bots. (sitemap.xml to the text)  

# luigid

* DOING daemon.ml[i] implementing basic daemonization.
* init.d deamon
* Image transform for web
  http://www.fmwconcepts.com/imagemagick/threshold_comparison/index.php
* Images for the web
** http://optipng.sourceforge.net/pngtech/optipng.html
**  zc = 9  zm = 9  zs = 3  f = 5
** mogrify -format png -resample 100x100 Reveil\ A_1925_05_01_F_000*.tif
** Browser image scaling
** http://stackoverflow.com/questions/2303690/resizing-an-image-in-an-html5-canvas

* Spell-checking according to the data in the corpus. 
** Combination of N-grams + edit distance.
** http://norvig.com/spell-correct.html
** http://www.dcs.bbk.ac.uk/~roger/spellchecking.html

* pdf compression G4 vs jbig2
http://digit.nkp.cz/knihcin/digit/vav/bi-level/compression_bi-level_images.html

* Nginx rate limiting to avoid scrape
*** http://sysarcana.com/2010/05/26/rate-limiting-with-nginx/
*** http://wiki.nginx.org/NginxHttpLimitReqModule
    
* Finding rectangles 
** http://twanvl.nl/blog/haskell/finding-rectangles
** http://twanvl.nl/blog/haskell/finding-rectangles-part2

* Convert timings
finereader -> pbin 53min (2 workers *)
finereader -> pbin 48min (3 workers *)
pbin -> ptxt 7min
tiff -> bilevel png 9h30 (single cores)
png optim -> 4h30 (two cores)

* PNG 4.2 Go (imagemagick) 3.7 Go (optim)
* Cira catalogue ISBD + id interne => export xmlmba
