==================================================================
Flickr User-POI Visits (Melbourne)
==================================================================

Dataset Information: 
This dataset comprises a set of users and their visits to various points-of-interest (POIs) in Melbourne, with a total of 3975 tours and 17,087 visits. The user-POI visits are determined based on geo-tagged YFCC100M Flickr photos that are: (i) mapped to specific POIs location and POI categories; and (ii) grouped into individual travel sequences (consecutive user-POI visits that differ by <8hrs). 

All user-POI visits in each city are stored in "userVisits-Melb-allPOI.csv" that contains the following columns/fields:
 - photoID: identifier of the photo based on Flickr.
 - userID: identifier of the user based on Flickr.
 - dateTaken: the date/time that the photo was taken (unix timestamp format).
 - poiID: identifier of the place-of-interest (Flickr photos are mapped to POIs based on their lat/long).
 - poiTheme: category of the POI (e.g., Park, Museum, Cultural, etc).
 - poiFreq: number of times this POI has been visited.
 - seqID: travel sequence no. (consecutive POI visits by the same user that differ by <8hrs are grouped as one travel sequence).

In addition, the list of POIs can be found in "POI-Melb.csv", along with their POI category (theme), sub-category (subTheme) and lat/long coordinates.
 
------------------------------------------------------------------
References / Citations
------------------------------------------------------------------
If you use this dataset, please cite the following paper:
 - Xiaoting Wang, Christopher Leckie, Jeffery Chan, Kwan Hui Lim and Tharshan Vaithianathan. "Improving Personalized Trip Recommendation to Avoid Crowds Using Pedestrian Sensor Data". In Proceedings of the 25th ACM International Conference on Information and Knowledge Management (CIKM'16). Pg 25-34. Oct 2016.

The corresponding bibtex for this paper is as follows:
 @INPROCEEDINGS { wang-cikm16,
	AUTHOR = {Xiaoting Wang and Christopher Leckie and Jeffery Chan and Kwan Hui Lim and Tharshan Vaithianathan},
	TITLE = {Improving Personalized Trip Recommendation to Avoid Crowds Using Pedestrian Sensor Data},
	BOOKTITLE = {Proceedings of the 25th ACM International Conference on Information and Knowledge Management (CIKM'16)},
	PAGES = {25-34},
	YEAR = {2016}
 }