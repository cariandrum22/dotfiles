import sys
import platform
import subprocess
import http.client
import json

MARKETPLACE_DOMAIN = "marketplace.visualstudio.com"
ASSETS_DOMAIN = "{publisher}.gallery.vsassets.io"
EXTENSION_QUERY_ENDPOINT = "/_apis/public/gallery/extensionquery"
DOWNLOAD_ENDPOINT = (
    "/_apis/public/gallery/publisher/{publisher}"
    "/extension/{extension}/{version}"
    "/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage{queryString}"
)


def open_extension_file(file):
    with open(file, "r") as file:
        lines = list(map(str.strip, file))
        return lines


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


def results_to_nix_attr(publisher, name, version, sha256, arch=None):
    return f"""
  {{
    name = "{name}";
    publisher = "{publisher}";
    version = "{version}";
    sha256 = "{sha256}";
    {'arch = "' + arch + '";' if arch is not None else ''}
  }}"""


def convert_system_arch_representation():
    system = platform.system().lower()
    machine = platform.machine()
    if machine == "x86_64":
        return f"{system}-x64"
    elif machine == "i686":
        return  f"{system}-x86"
    elif machine == "aarch64" or machine == "arm64":
        return  f"{system}-arm64"
    elif machine == "armv7l":
        return  f"{system}-armhf"


def extract_version_and_platform(versions):
    for version in versions:
        if version.get("targetPlatform") == convert_system_arch_representation():
            return version["version"], version["targetPlatform"]
    for version in versions:
        if "targetPlatform" not in version:
            return version["version"], None
    return None, None

def extract_extension_info(result):
    extension = result["body"]["results"][0]["extensions"][0]
    publisher = extension["publisher"]["publisherName"]
    name = extension["extensionName"]
    version, arch = extract_version_and_platform(extension["versions"])
    
    if version is None:
        raise ValueError(f"No compatible version found for extension {publisher}.{name}")
    
    queryString = ""
    if arch:
        queryString = f"?targetPlatform={arch}"

    sha256 = (
        subprocess.run(
            [
                "nix-prefetch-url",
                "https://"
                + ASSETS_DOMAIN.format(publisher=publisher)
                + DOWNLOAD_ENDPOINT.format(
                    publisher=publisher, extension=name, version=version, queryString=queryString
                ),
            ],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
        )
    ).stdout.strip()
    return results_to_nix_attr(publisher, name, version, sha256, arch)


def main():
    print("Updating VSCode extensions...")

    results = list(map(query, parse_extension_list(open_extension_file("extensions"))))

    extensions_nix = "".join([extract_extension_info(result) for result in results])
    with open("extensions.nix", "w") as file:
        file.write("[")
        file.write(extensions_nix)
        file.write("\n]\n")

    print("Done.")


if __name__ == "__main__":
    sys.exit(main())
