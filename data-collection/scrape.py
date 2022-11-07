from bs4 import BeautifulSoup
# import json
import urllib3
# import re

http = urllib3.PoolManager()
# response_imdb = http.request("GET", 'https://www.imdb.com/title/tt0114709/')
# response_mojo = http.request("GET", "https://www.boxofficemojo.com/title/tt0114709/")
response_imdb = http.request("GET", 'https://www.imdb.com/title/tt15474916/')
response_mojo = http.request("GET", "https://www.boxofficemojo.com/title/tt15474916/")

html_doc1 = response_imdb.data.decode('utf-8')
html_doc2 = response_mojo.data.decode('utf-8')
soup_imdb = BeautifulSoup(html_doc1)
soup_mojo = BeautifulSoup(html_doc2)

"""
-- Title: imdb
-- Year/Release Date: mojo
-- Revenue: imdb
Critics rating: mojo Where do I find this?
-- BUDGET: imdb
-- Names of cast (Count of golden globe / oscar award winning actors): imdb
-- Director (Count of award winning directors): imdb
Music Director (?)
- Genre(s): imdb
-- Duration: mojo
Maturity rating: imdb (certificate)
-- Production company: imdb
Distributor
-- Country of origin: imdb
Franchise status
"""
resp = {
    "title": soup_imdb.find("h1", attrs={"data-testid": "hero-title-block__title"}).get_text(),
    "release_date": soup_imdb.find("li", attrs={"data-testid": "title-details-releasedate"}).get_text(),
    "country": soup_imdb.find("li", attrs={"data-testid": "title-details-origin"}).get_text(),
    "production_companies": list(
        map(lambda e: e.get_text()
        , soup_imdb.find("li", attrs={"data-testid": "title-details-companies"})
                    .find("div")
                    .find("ul")
                    .find_all("li")
        )
    ),
    "budget": (soup_imdb.find("li", attrs={"data-testid": "title-boxoffice-budget"})
                    .find("div")
                    .find("ul")
                    .find("li").get_text()
    ),
    "revenue": soup_mojo.find("span", text="Domestic Opening").nextSibling.get_text(),
    # "genres": list(
    #     map(lambda e: e.get_text(),
    #      soup_imdb.find("li", attrs={"data-testid": "storyline-genres"})
    #                 .find("div")
    #                 .find("ul")
    #                 .find_all("li")
    #     )
    # ), # this doesn't work on imdb for some reason, using the genres from mojo even though they aren't list separated
    "genres": soup_mojo.find("span", text="Genres").nextSibling.get_text().split(),
    "duration": soup_mojo.find("span", text="Running Time").nextSibling.get_text(),
    "actors": list(
        map(lambda e: e.get_text(),
         soup_imdb.find("div", attrs={"data-testid": "title-cast-header"})
                    .nextSibling
                    .find_all("a", attrs={"data-testid": "title-cast-item__actor"})
        )
    ),
    "director": soup_imdb.find("section", attrs={"data-testid": "title-cast"})
                        .find("span", text="Director").nextSibling.get_text(),
}

# TODO:
# - fetch list of imdb titles
# - for-loop to create `resp` dict for each movie
# - save as csv or json
# - minor cleaning may be required, TBD after scraping ends
print(resp)