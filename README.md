## NHL PbP Scraper and Expected Goals Model

This code was created to use Emmanuel Perry's of [Corsica](corsica.hockey) play by play scraper to scrape daily NHL games and apply
my Expected Goals model to describe the quality of chances each team had in the game.  My model can be
read about [here](https://rpubs.com/mcbarlowe/xGmodel).  If you have any questions feel free to email
me at the address in my profile or post a bug on github.

Included is a python script to read from various text files and post the contents to twitter through an api.  The scripts reads in
the keys from a text file using this format `key_name: key`.  It also takes command line arguments detailed in the doc string
of the script.

Stats the scraper compiles include CF, CA, CF%, FF, FA, FF%, GF, GA, G, A1, A2, xGF, xGA, xGF%.
This includes the rate stats for G, A, Corsi, Fenwick, xG.  These stats are compiled at both a
player level and a team level both 5v5/all situations plus adjusted for score/venue and raw.
