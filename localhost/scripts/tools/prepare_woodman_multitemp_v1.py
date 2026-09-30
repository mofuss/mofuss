"""Extract Woodman annual land-cover and conservative TOF maps in one pass.

The Woodman archive is pixel-interleaved across 141 bands. Reading the selected
2000-2050 bands together avoids decompressing the global source 51 times.
Output codes are documented in global_growth/luc_woodman_categories.csv.
"""

from __future__ import annotations

import argparse
import json
import os
from contextlib import ExitStack
from pathlib import Path

import numpy as np
import rasterio
from rasterio.windows import Window


SOURCE_TO_MODEL = {0: 0, 11: 11, 22: 22, 33: 33, 441: 44,
                   442: 45, 55: 55, 66: 66, 77: 77}
TOF_MODEL_CODES = (11, 66, 77)
VALID_SOURCE_CODES = set(SOURCE_TO_MODEL)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--first-year", type=int, default=2000)
    parser.add_argument("--last-year", type=int, default=2050)
    parser.add_argument("--rows", type=int, default=16)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.first_year < 1960 or args.last_year > 2100 or args.first_year > args.last_year:
        raise ValueError("Requested years must be within 1960-2100, in ascending order")
    if args.rows < 1 or args.rows > 256:
        raise ValueError("--rows must be between 1 and 256")
    years = list(range(args.first_year, args.last_year + 1))
    args.output_dir.mkdir(parents=True, exist_ok=True)
    final = {
        (year, kind): args.output_dir / f"woodman_{kind}_{year}_gcs.tif"
        for year in years for kind in ("luc", "tof")
    }
    part = {key: path.with_name(path.name + ".part") for key, path in final.items()}
    existing = [path for path in (*final.values(), *part.values()) if path.exists()]
    if existing:
        raise FileExistsError(
            "Refusing to overwrite an existing annual file; first example: "
            + str(existing[0])
        )

    lookup = np.zeros(max(VALID_SOURCE_CODES) + 1, dtype=np.uint8)
    for source_code, model_code in SOURCE_TO_MODEL.items():
        lookup[source_code] = model_code

    with rasterio.Env(GDAL_CACHEMAX=256), rasterio.open(args.source) as src:
        if src.count != 141 or src.width != 36000 or src.height != 18000:
            raise ValueError("Unexpected Woodman source shape/band count")
        if src.crs is None or src.crs.to_epsg() != 4326:
            raise ValueError("Woodman source must be EPSG:4326")
        bands = [year - 1959 for year in years]
        for year, band in zip(years, bands):
            if src.descriptions[band - 1] != str(year):
                raise ValueError(f"Band {band} is not labelled {year}")

        profile = src.profile.copy()
        profile.update(
            count=1, dtype="uint8", nodata=0, tiled=False,
            blockysize=args.rows, compress="DEFLATE", predictor=2,
            zlevel=6, BIGTIFF="IF_SAFER", interleave="band",
        )
        tof_profile = profile.copy()
        tof_profile["nodata"] = 255

        with ExitStack() as stack:
            writers = {
                (year, kind): stack.enter_context(
                    rasterio.open(part[(year, kind)], "w", **(
                        profile if kind == "luc" else tof_profile
                    ))
                )
                for year in years for kind in ("luc", "tof")
            }
            for row in range(0, src.height, args.rows):
                height = min(args.rows, src.height - row)
                window = Window(0, row, src.width, height)
                raw = src.read(indexes=bands, window=window)
                np.nan_to_num(raw, copy=False, nan=0.0)
                source_codes = raw.astype(np.uint16)
                unexpected = set(np.unique(source_codes)) - VALID_SOURCE_CODES
                if unexpected:
                    raise ValueError(f"Unexpected Woodman class at row {row}: {unexpected}")
                luc = lookup[source_codes]
                for index, year in enumerate(years):
                    annual_luc = luc[index]
                    writers[(year, "luc")].write(annual_luc, 1, window=window)
                    annual_tof = np.full(annual_luc.shape, 255, dtype=np.uint8)
                    annual_tof[annual_luc != 0] = 0
                    for code in TOF_MODEL_CODES:
                        annual_tof[annual_luc == code] = 1
                    writers[(year, "tof")].write(annual_tof, 1, window=window)
                if row == 0 or (row // args.rows) % 100 == 0:
                    print(f"Processed rows {row + height:,}/{src.height:,}", flush=True)

    for key, path in final.items():
        os.replace(part[key], path)
    manifest = {
        "source": str(args.source.resolve()),
        "scenario": args.source.stem,
        "first_year": args.first_year,
        "last_year": args.last_year,
        "source_to_model": SOURCE_TO_MODEL,
        "tof_model_codes": TOF_MODEL_CODES,
        "ocean_and_nan": "LULC nodata 0; TOF nodata 255",
        "files": {f"{year}_{kind}": str(path.name) for (year, kind), path in final.items()},
    }
    (args.output_dir / "woodman_series_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"Wrote {len(final)} annual rasters to {args.output_dir}")


if __name__ == "__main__":
    main()
