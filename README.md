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

**NOTE:** Some of the scripts make use of `pbmcapply::pbmclapply()` (please find the project [here](https://github.com/kvnkuang/pbmcapply)), which does not work on Windows. For a parallelized `lapply()`-solution on Windows, e.g. look into `parallel::parLapply()` (see [here](https://gradientdescending.com/simple-parallel-processing-in-r/) for a brief guide). We plan on generalizing the scraping scripts to be platform-agnostic in the near future using [the furrr-package](https://furrr.futureverse.org/) for parallel computation; the idea is already implemented in the `R/taz_full.R` script.

*Special thanks to the [University of Münster](https://www.uni-muenster.de/de/) for providing us with additional computational resources for this project.*

## Contributing

Currently, all scrapes work as intended. Yet, there are multiple points where contributions or input are much appreciated:

* **Code efficiency:** In all scripts, the `magrittr`-pipe operator `%>%` should be replaced with the base R `|>`. This may sometimes require rewriting small sections of code because of slight differences between the two pipes. The reasoning for doing this is that the base R pipe is slightly faster & the traceback is much cleaner. *Note: this should be done with R 4.2+ in mind in terms of style.*

* **Generalizing parallel computations using furrr:** Currently, some scripts use `pbmcapply::pbmclapply()`, which does not work on Windows. Instead of targetting Windows separately, we should simply use the `furrr`-package which is platform-agnostic. I have implemented this in `R/taz_full.R` and this approach could simply be transferred to the other scripts. See [here](https://furrr.futureverse.org/) for an introduction to `furrr`, and [here](https://furrr.futureverse.org/articles/progress.html) for an introduction to `progressr`, which I think is a convenient solution for handling progress notifications in this case.

* **General cleanup:** Here, multiple things could probably be improved:
  - Rewrite sections of code that may be ambivalent, inefficient or intransparent (especially the `full_scrape()` parts).
  - Explore potentials to make the scraping more "gentle", since we are rapidly sending high numbers of requests. 
  - Replace `purrr`-style anonymous function shorthands with the base R equivalent (i.e. `data |> map(\(.x) lm(x ~ y, .x))` instead of `data |> map(~ lm(x ~ y, .x))`). This is optional and probably a style question, but I think now that base R supports this we should not intermix too many different syntax styles.
  
* **Adding more news sites:** This is of course also possible and appreciated. Look to the existing scripts for some guidance. We could also think about including english-language sites at some point.

* **Adding more examples:** This data is very versatile and enables a lot of interesting analyses. More examples would always be appreciated.

* **Hosting the data set:** The resulting data sets can be quite large, and it would be nice to host them somewhere so anyone interested can simply download the data without having to scrape it first.

Feel free to simply create pull requests if you want to address any of this points, or to create issues if you want to contribute but need help or guidance.
