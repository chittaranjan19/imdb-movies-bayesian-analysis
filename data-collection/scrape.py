from bs4 import BeautifulSoup
# import json
import urllib3
urllib3.disable_warnings()
import pandas as pd
import time
import re

movies = pd.read_csv("../ml-25m/movies.csv", dtype=str)
links = pd.read_csv("../ml-25m/links.csv", dtype=str)
movies_links = movies.merge(links, on="movieId")
http = urllib3.PoolManager()

def get_info(row):
    global http
    imdb_id = row["imdbId"]
    genres = row["genres"].split("|")

    response_imdb = http.request("GET", f"https://www.imdb.com/title/tt{imdb_id}/")
    response_mojo = http.request("GET", f"https://www.boxofficemojo.com/title/tt{imdb_id}/")

    html_doc1 = response_imdb.data.decode('utf-8')
    html_doc2 = response_mojo.data.decode('utf-8')
    soup_imdb = BeautifulSoup(html_doc1, features="lxml")
    soup_mojo = BeautifulSoup(html_doc2, features="lxml")

    """
    -- Title: movielens
    -- Year/Release Date: mojo
    -- Revenue: imdb
    Critics rating: mojo Where do I find this?
    -- BUDGET: imdb
    -- Names of cast (Count of golden globe / oscar award winning actors): imdb
    -- Director (Count of award winning directors): imdb
    Music Director (?)
    -- Genre(s): movielens
    -- Duration: mojo
    Maturity rating: imdb (certificate)
    -- Production company: imdb
    Distributor
    -- Country of origin: imdb
    Franchise status
    """
    d = {
        "imdb_id": imdb_id,
        "title": row["title"],
        "genres": ";".join(genres)
    }
    release_date = soup_imdb.find("li", attrs={"data-testid": "title-details-releasedate"})
    if release_date:
        release_date = release_date.get_text()
    d["release_date"] = release_date
    country = soup_imdb.find("li", attrs={"data-testid": "title-details-origin"})
    if country:
        country = country.get_text()
    d["country"] = country
    production_companies = soup_imdb.find("li", attrs={"data-testid": "title-details-companies"})
    if production_companies:
        production_companies = ";".join(list(
            map(lambda e: e.get_text(),
            production_companies.find("div").find("ul").find_all("li")
            )))
    d["production_companies"] = production_companies
    budget = soup_imdb.find("li", attrs={"data-testid": "title-boxoffice-budget"})
    if budget:
        budget = budget.find("div").find("ul").find("li").get_text()
    d["budget"] = budget
    revenue = soup_mojo.find("span", text="Domestic Opening")
    if revenue:
        revenue = revenue.nextSibling.get_text()
    d["revenue"] = revenue
    duration = soup_mojo.find("span", text="Running Time")
    if duration:
        duration = duration.nextSibling.get_text()
    d["duration"] = duration
    actors = soup_imdb.find("div", attrs={"data-testid": "title-cast-header"})
    if actors:
        actors = ";".join(
            list(
                map(
                    lambda e: e.get_text(), 
                    actors.nextSibling.find_all("a", attrs={"data-testid": "title-cast-item__actor"})
                )
            )
        )
    d["actors"] = actors
    director = soup_imdb.find("section", attrs={"data-testid": "title-cast"})
    if director:
        director = ";".join(
            list(
                map(
                    lambda e: e.get_text(),
                     director.find("span", text=re.compile("Director")).nextSibling.find_all("li")
                )
            )
        )
    d["director"] = director
    return d

# TODO:
# - fetch list of imdb titles
# - for-loop to create `resp` dict for each movie
# - save as csv or json
# - minor cleaning may be required, TBD after scraping ends

df = pd.DataFrame(
    {
        "imdb_id": [], 
        "title": [], 
        "release_date": [], 
        "country": [],
        "production_companies": [],
        "budget": [],
        "revenue": [], 
        "genres": [], 
        "duration": [], 
        "actors": [], 
        "director": []
    }
)
df.to_csv("data.csv", index=False)
out = open("out", "w")
for _, row in movies_links.head(20).iterrows():
    print(_, row["imdbId"], file=out, flush=True)
    try:
        d = get_info(row)
        pd.Series(d).to_frame().T.to_csv("data.csv", mode="a", index=False, header=False)
    except:
        print("ERROR", row["imdbId"], file=out, flush=True)
        continue
# movies_links.head().apply(get_info, axis=1).apply(add_dict_to_df, axis=1)
# print(df)