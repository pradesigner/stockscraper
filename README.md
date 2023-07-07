# stockscraper

stockscraper is used to identify vegan stocks on various exchanges.

finance.yahoo profile descriptions are searched for keywords contained in yay, nay and may files as created by brittany. the frequency of each word in each list are totalled, giving yay, nay and may column counts.

the data is sorted by the yay column, so as to give priority to yay items regardless of nay|may counts.

the yay, nay, may keywords are highlighted within the descriptions for context.

## notes

### 503 error
a major problem was dealing with 503 error stemming from finance.yahoo not responding to request by enlive's html-resource. thx to sean corfield, the ubiquitous clojure guru, we learned how to stacktrace and discovered that the problem stemmed from html-resource not doing any error checking at all. therefore, the fetch-url program was re-written with a loop utilizing html-snippet and the slurper (which slurps continuously regardless of what is returned).

### useful links
exchange lists from http://eoddata.com/  
stock descriptions from https://finance.yahoo.com/

## Usage

(main <EXCH>)
or
(map main exchs) but then progress viewing is suppressed.

## License

Copyleft © 2020
