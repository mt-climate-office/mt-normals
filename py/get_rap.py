# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "httpx",
# ]
# ///

import httpx
import json
from typing import Literal, Any


baseURL = "https://us-central1-rap-data-365417.cloudfunctions.net"

def get_rap_data(endpoint: Literal["productionV3", "coverV3"], polygon: dict[Any, Any]):
    out = httpx.post(
        f"{baseURL}/{endpoint}",
        headers = {"Content-Type": "application/json'"},
        data=polygon
    )
    return out

if __name__ == "__main__":
    with open("../data/regions/counties.geojson", "r") as f:
        gj = json.load(f)

    for feature in gj["features"]:
        dat= get_rap_data("productionV3", feature)
        print(dat)
        break