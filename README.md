# Scraping German News Websites

Scripts to scrape large German news websites, used for my project on automating news media bias detection. To get the code, simply run 

```
git clone https://github.com/kssrr/german-media-scrape
```

If you are unfamiliar with git, you can copy-paste & run the `setup.R`-script, which will also install the dependencies for you. 

### Getting the data

A demo data set can be downloaded [here](https://archive.org/details/german-media-scrape_202302), at archive.org. It includes scrapes for TAZ, Zeit, Süddeutsche Zeitung, Spiegel & WELT (all articles from January 01st 2020 to December 31st 2022). The uncompressed data is around 3.5GB. Some sites lack article titles; this is due to an earlier problem & we will add them back in later versions.

## Examples

You can do a lot of interesting exploratory analysis with this kind of data, for example examine reporting on political parties:

![Reporting on political parties on two German news websites.](https://user-images.githubusercontent.com/121236725/210736182-01f7a3f2-3f72-420c-b03e-8cc252426dba.png)

You could also look at the salience of particular topics:

![Media Attention on Ukraine after the 2022 invasion.](https://user-images.githubusercontent.com/121236725/210806381-59ed1d41-fac2-4b1f-a99d-748e30a428ef.png)

![World Cup 2022](https://user-images.githubusercontent.com/121236725/210814418-e05d6aaf-5976-454f-89af-d5e9998476df.png)

Or investigate pairwise correlation clusters of keywords (click to enlarge; see [here](https://www.tidytextmining.com/ngrams.html#counting-and-correlating-pairs-of-words-with-the-widyr-package) for the methodology):

![Network](https://user-images.githubusercontent.com/121236725/212491454-24f43fbc-788f-4496-878f-c5068d68f89a.png)

![welt_network](https://user-images.githubusercontent.com/121236725/212496053-41494d26-1043-477c-b23a-3d29fe73613a.png)

*Special thanks to the [University of Münster](https://www.uni-muenster.de/de/) for providing us with additional computational resources for this project.*

