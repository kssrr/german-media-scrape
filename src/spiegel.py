import json
import re
import requests
import unicodedata

from bs4 import BeautifulSoup
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, asdict
from typing import Optional
from tqdm import tqdm

BASE_URL: str = "https://www.spiegel.de/sitemap.xml"
YEARS: list[str] = ["2022", "2023"]

@dataclass
class Article:
    url: str
    title: Optional[str] = None
    date: Optional[str] = None
    description: Optional[str] = None
    keywords: Optional[str] = None
    author: Optional[str] = None
    paywall: Optional[bool] = None
    body: Optional[str] = None
    error: Optional[str] = None

# find the relevant sitemaps/archives from the overall sitemap:

pattern: str = "|".join([f"sitemap-{year}-" for year in YEARS])
sitemap = BeautifulSoup(requests.get(BASE_URL).content, "xml")
sitemaps: list[str] = [url for url in [elem.get_text() for elem in sitemap.find_all("loc")] if re.search(pattern, url)]

# get URLs of all relevant articles:

def articles(smap: str, session: requests.Session) -> list[str]:
    resp: requests.models.Response = session.get(smap, timeout=10)
    xml = BeautifulSoup(resp.content, "xml")

    return [loc.get_text() for article in xml.find_all("url") if (loc := article.find("loc")) is not None]

print(f"Checking {len(sitemaps)} archive collections for articles to scrape...\n")

article_urls: list[str] = []
session = requests.Session()

with ThreadPoolExecutor(max_workers=3) as executor:
    future_to_smap = {
        executor.submit(articles, smap, session): smap
        for smap in sitemaps
    }

    for future in tqdm(as_completed(future_to_smap), total=len(sitemaps)):
        smap = future_to_smap[future]
        try:
            article_urls.extend(future.result())
        except Exception as e:
            print(f"Failed on {smap}: {e}")

print(f"Found {len(article_urls)} articles to scrape.\n")

# scraping procedure for articles ----

# return None instead of empty string/whitespace:
def guard(text: Optional[str]) -> Optional[str]:
    if text is None or not text.strip():
        return None
    return text


def paywalled(html: str) -> bool:
    return bool(re.search(r'paywall":\{"attributes":\{"is_active":true\}', html))


def get_title(soup: BeautifulSoup) -> Optional[str]:
    tag = soup.find("title")
    return guard(tag.get_text(strip=True) if tag else None)


def get_body(soup: BeautifulSoup) -> Optional[str]:
    paragraphs = [p.get_text(separator=" ", strip=True) for p in soup.find_all("p")]
    return guard(unicodedata.normalize("NFKC", " ".join(paragraphs[:-2]))) # drop last 2 (footer/boilerplate)


def get_meta(soup: BeautifulSoup, name: str) -> Optional[str]:
    tag = soup.find(attrs={"name": name})
    return guard(tag.get("content") if tag else None)


def scrape_article(url: str, session: requests.Session) -> Article:
    resp = session.get(url, timeout=10)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.content, "lxml")

    paywall = paywalled(resp.text)

    return Article(
        url=url,
        title=get_title(soup),
        date=get_meta(soup, "date"),
        description=get_meta(soup, "description"),
        keywords=get_meta(soup, "keywords"),
        author=get_meta(soup, "author"),
        paywall=paywall,
        body=None if paywall else get_body(soup),
    )


def scrape_safely(url: str, session: requests.Session) -> Article:
    try:
        return scrape_article(url, session)
    except Exception as e:
        return Article(url=url, error=str(e))

# actually pull them ----

print(f"Pulling articles...\n")

session = requests.Session()
articles: list[Article] = []

with ThreadPoolExecutor(max_workers=3) as executor:
    futures = {executor.submit(scrape_safely, url, session): url for url in article_urls}
    for future in tqdm(as_completed(futures), total=len(article_urls)):
        articles.append(future.result())  # scrape_safely never raises

print(f"Dumping to JSON...\n")

with open("spiegel.json", "w", encoding="utf-8") as f:
    json.dump([asdict(a) for a in articles], f, ensure_ascii=False, indent=2)

print("All done.\n")