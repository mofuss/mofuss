# M85_B30_V1 regionalization

`M85_B30_V1` is the minimum disjoint Global South regionalization needed to
contain the 30 accepted directed bilateral charcoal-flow permissions in
`bilateral_charcoal_permissions_v1.csv`.

## Design

- A directed permission `supplier > importer` means the importing country's V
  demand may source from its own territory and from that named supplier.
- Permissions are not made reciprocal and are not expanded transitively.
- W demand is always restricted to its country of origin.
- Evidence tiers determine which V edges are admitted. `ABC` is the default;
  `A` and `AB` are supported sensitivity policies.
- Region membership is only the computational landscape. It does not itself
  grant harvesting access.
- Countries are joined using the undirected form of the accepted edge graph.
  Its connected components are therefore the smallest non-overlapping regions
  that contain every accepted flow. Countries without an accepted edge remain
  singletons.

## Result

- 111 demand-ready countries
- 85 computational regions
- 9 multi-country evidence-connected components
- 76 domestic-only singletons
- largest component: 13 countries

| RunCode | Countries | Accepted directed edges |
|---|---:|---:|
| ECSA | 13 | 16 |
| MSEA | 6 | 5 |
| GOG4 | 4 | 3 |
| ARPY | 2 | 1 |
| BOBR | 2 | 1 |
| CMCG | 2 | 1 |
| HISP | 2 | 1 |
| BNNG | 2 | 1 |
| GMSN | 2 | 1 |

The ECSA component is large because the accepted links form one connected chain
through COD, East Africa, Zambia, Mozambique, southern Africa, and neighbouring
importers. Splitting it into disjoint regional runs would break at least one
accepted bilateral permission.

## Reproducibility

Run `build_M85_B30_V1.R` from this directory after changing the permission
table. The builder fails if the evidence graph no longer matches the named
multi-country components, if an endpoint falls outside the 111-country demand
extent, or if an accepted edge crosses a generated region.

The `SourceDocumentIDs` column connects every model permission to the preserved
source register and claim locators in
`E:/MoFuSS_Global_South_Regionalization_Evidence_Archive_v1_2026-08-29`.
