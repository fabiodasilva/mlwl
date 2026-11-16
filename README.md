# mlwl — Machine Learning Warehouse Location

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3941119.svg)](https://doi.org/10.5281/zenodo.3941119)
![R](https://img.shields.io/badge/language-R-276DC3)
![License: GPL-3.0](https://img.shields.io/badge/license-GPL--3.0-blue)

Code and data supplement to my Professional Master's dissertation at the **University of São Paulo (USP)**,
*"Seleção da localização de armazéns frigorificados para operações de distribuição através do agrupamento das distâncias dirigidas"*
(Selecting cold-storage warehouse locations for distribution operations by clustering driving distances), 2020.
Advisor: Prof. Dr. Francisco Louzada Neto.

## Problem

Where should a cold-chain logistics operator place refrigerated warehouses to serve demand across **all 5,570 Brazilian municipalities**?

Classic facility-location models typically rely on straight-line (Euclidean / great-circle) distances. In a country with Brazil's
road network, geography and regional disparities, straight-line proximity is a poor proxy for real delivery cost.
This project tests a data-driven alternative: **segment the market using actual road (driving) distances, then place one
warehouse per segment, weighted by estimated cold-storage demand.**

## Approach

```
5,570 municipalities ──► demand estimate per city ──► 5,570 × 5,570 driving-distance matrix (OSRM)
        │                                                          │
        │                                                          ▼
        │                                         k-means (k = 50) on distance profiles
        │                                                          │
        ▼                                                          ▼
 IBGE + POF data                         demand-weighted geographic midpoint per cluster
                                                                   │
                                                                   ▼
                                  reverse geocoding ► candidate warehouse sites
                                                                   │
                                                                   ▼
                       "reverse" driving distances city → site ► demand coverage within 150 km
```

1. **Demand model** — each municipality gets an estimated cold-storage requirement (`CapEstNem3`, in m³) derived from
   urban population, IBGE regional data and household food-consumption data (POF).
2. **Driving-distance matrix** — a full 5,570 × 5,570 origin-destination matrix computed against a self-hosted
   [OSRM](http://project-osrm.org/) routing server (≈31 million pairs). Unreachable pairs are set to 9,000 km
   (above the observed maximum of ~7,590 km) so they are never grouped together.
3. **Clustering** — `kmeans` (Lloyd, k = 50, up to 100 iterations) using each city's vector of driving distances to every
   other city as its feature profile. Cities that are *close by road to the same places* end up in the same segment.
4. **Site selection** — for each cluster, a geographic midpoint weighted by demand (m³) is computed and reverse-geocoded
   (OpenStreetMap / `tmaptools`) to a real locality.
5. **Validation** — driving distances from each city back to its cluster's site are recomputed, and the demand served
   within **150 km** is summed per site to estimate each warehouse's addressable volume.

## Repository contents

| File | Description |
|---|---|
| `nagrupa.R` | Full pipeline: data load, OSRM distance matrix, k-means, weighted midpoints, maps, coverage per site |
| `cidades.csv` | Dataset with 5,570 municipalities (`;`-separated, decimal comma) |
| `LICENSE` | GPL-3.0 |

Key columns in `cidades.csv`:

| Column | Meaning |
|---|---|
| `nome_mun`, `CD_GEOCODI`, `uf` | Municipality name, IBGE code, state |
| `cod_rgi` / `nome_rgi`, `cod_rgint` / `nome_rgint` | IBGE immediate and intermediate geographic regions |
| `idhm` | Municipal Human Development Index |
| `pop_urbana` | Urban population |
| `KgFrioUF`, `POFKgmes`, `POF_FC`, `AFCMes`, `PalleteMes`, `CapEstNepp` | Intermediate demand variables (refrigerated food consumption → pallets → storage) |
| `CapEstNem3` | **Estimated cold-storage capacity required (m³)** — the weight used for site selection |
| `lat`, `lon` | Municipality coordinates |

## Reproducing

Requirements: R (≥ 3.6), packages `osrm`, `dplyr`, `maps`, `tmaptools`, and an OSRM server loaded with the Brazil OSM extract.

```r
install.packages(c("osrm", "dplyr", "maps", "tmaptools"))
options(osrm.server = "http://localhost:5000/")   # point to your OSRM instance
source("nagrupa.R")
```

The full distance matrix is the expensive step; it requires a local OSRM instance (the public demo server will not accept
a request of this size).

## Results

- **50 candidate cold-storage sites** covering all 5,570 Brazilian municipalities, one per driving-distance segment.
- For each site, the script outputs the list of municipalities it serves, ordered by driving distance (`rc<N>dd.csv`),
  and the cold-storage demand (m³) addressable within **150 km**, consolidated across all sites.

### Why driving distance matters

Route CEASA-SP → CEASA-RJ (São Paulo and Rio de Janeiro wholesale food hubs):

| Distance metric | km |
|---|---|
| Driving distance (OSRM) | **425.2** |
| Euclidean (eqs. 2.2–2.4) | 356.15 |
| Great-circle / haversine (eqs. 2.5–2.6) | 356.15 |

The road route is **~19% longer** than either straight-line metric. That gap is not academic: trucks travel the road
network, so **freight is charged and delivery windows are scheduled on driving distance**. A location model built on
straight-line distances underestimates transport cost and transit time by a margin that varies from region to region,
which shifts where a warehouse should be placed. Clustering on driving distances keeps the model aligned with the
quantities the operation actually pays for and commits to.

## Limitations and known gaps

- `geographic_midpoint.R` (sourced by the script) and `agrupamentosKDD.csv` (site list after manual cleanup of
  reverse-geocoding results) are **not yet in the repository**, so steps 4–5 are not fully reproducible as-is.
- The script was written as a research notebook: cluster handling is repeated 50 times rather than looped, and `k = 50`
  was fixed rather than selected by a criterion (elbow, silhouette, cost trade-off).
- k-means on raw distance rows is a heuristic. k-medoids/PAM or a p-median formulation would optimize the
  facility-location objective more directly. These are natural next steps.

## Citation

```bibtex
@software{silva_mlwl_2020,
  author    = {Silva, Fábio Fernando da},
  title     = {mlwl: Machine Learning Warehouse Location},
  year      = {2020},
  publisher = {Zenodo},
  version   = {v0.2},
  doi       = {10.5281/zenodo.3941119},
  url       = {https://doi.org/10.5281/zenodo.3941119}
}
```

## Author

**Fábio Fernando da Silva** — [LinkedIn](https://www.linkedin.com/in/fabiofdasilva/)
