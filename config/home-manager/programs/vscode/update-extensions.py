import sys
import http.client
import json
import subprocess

MARKETPLACE_DOMAIN = "marketplace.visualstudio.com"
ASSETS_DOMAIN = "{publisher}.gallery.vsassets.io"
EXTENSION_QUERY_ENDPOINT = "/_apis/public/gallery/extensionquery"
DOWNLOAD_ENDPOINT = (
    "/_apis/public/gallery/publisher/{publisher}"
    "/extension/{extension}/{version}"
    "/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage"
)


def open_extension_file(file):
    with open(file, "r") as file:
        lines = list(map(str.strip, file))
        return lines


def prepare_extension_list():
    extensions = open_extension_file("extensions")
    extensions_from_nixpkgs = open_extension_file("extensions-from-nixpkgs")
    return list(set(extensions) - set(extensions_from_nixpkgs))


def to_dict(item):
    publisher, name = item.split(".")
    return {"publisher": publisher, "name": name}


def parse_extension_list(extensions):
    return list(map(to_dict, extensions))


def query(extension):
    connection = http.client.HTTPSConnection(MARKETPLACE_DOMAIN)
    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json;api-version=3.0-preview.1",
    }
    data = {
        "filters": [
            {
                "criteria": [
                    {"filterType": 8, "value": "Microsoft.VisualStudio.Code"},
                    {
                        "filterType": 7,
                        "value": f"{extension['publisher']}.{extension['name']}",
                    },
                ],
                "pageNumber": 1,
                "pageSize": 100,
                "sortBy": 0,
                "sortOrder": 0,
            }
        ],
        "assetTypes": [],
        "flags": 0x200,  # Include latest version only
    }
    connection.request("POST", EXTENSION_QUERY_ENDPOINT, json.dumps(data), headers)
    response = connection.getresponse()
    result = {
        "status": response.status,
        "reason": response.reason,
        "body": json.loads(response.read()),
    }
    connection.close()
    return result


def results_to_nix_attr(publisher, name, version, sha256):
    return f"""
  {{
    name = "{name}";
    publisher = "{publisher}";
    version = "{version}";
    sha256 = "{sha256}";
  }}"""


def extract_extension_info(result):
    extension = result["body"]["results"][0]["extensions"][0]
    publisher = extension["publisher"]["publisherName"]
    name = extension["extensionName"]
    version = extension["versions"][0]["version"]
    sha256 = (
        subprocess.run(
            [
                "nix-prefetch-url",
                "https://"
                + ASSETS_DOMAIN.format(publisher=publisher)
                + DOWNLOAD_ENDPOINT.format(
                    publisher=publisher, extension=name, version=version
                ),
            ],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
        )
    ).stdout.strip()
    return results_to_nix_attr(publisher, name, version, sha256)


def main():
    print("Updating VSCode extensions...")

    results = list(map(query, parse_extension_list(prepare_extension_list())))

    extensions_nix = "".join([extract_extension_info(result) for result in results])
    with open("extensions.nix", "w") as file:
        file.write("[")
        file.write(extensions_nix)
        file.write("\n]\n")

    print("Done.")


if __name__ == "__main__":
    sys.exit(main())
