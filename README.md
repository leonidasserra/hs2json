# hs2json

Biblioteca Haskell para representar, renderizar e imprimir JSON de forma
legível, testada com QuickCheck. Baseada no tutorial *Construindo e Testando
uma Biblioteca Haskell* (adaptação de Real World Haskell, caps. 5 e 11).

## 🎥 Vídeo de apresentação

**▶️ [Assista aqui](link_do_video)**

## Requisitos

GHC 9.x e cabal-install 3.x (instalados via [GHCup](https://www.haskell.org/ghcup/)).

## Como usar

```bash
cabal build                     # compila biblioteca, executável e testes
cabal run hs2json               # demonstração dos renderizadores
cabal test                      # suíte QuickCheck
cabal test --enable-coverage    # relatório de cobertura (HPC)
```

## Arquitetura

| Módulo | Responsabilidade |
| --- | --- |
| `SimpleJSON` | Tipo `JValue` e accessors seguros (`getString`, `getInt`, ...) |
| `PutJSON` | Renderizador ingênuo direto para `String` |
| `Prettify` | Biblioteca genérica de pretty printing sobre o tipo `Doc` |
| `PrettyJSON` | Renderização de `JValue` para `Doc`, com escapes Unicode |
| `QuickTestes` | Exemplos introdutórios de QuickCheck (qsort, `Ternary`) |
| `test/Spec.hs` | Instâncias `Arbitrary` e propriedades executadas via `quickCheckAll` |

## Decisões de projeto

- `nest` e `block` foram adicionados ao `Prettify` para indentar objetos e arrays.
- `compact` escolhe o lado achatado de cada `Union`, gerando JSON em uma linha.
- O gerador de `JValue` usa `resize (n `div` 2)` em volta de `listOf` para
  limitar a largura da árvore e evitar estouro de memória.

## 👥 Créditos

**Alunos:**

- **José Nunes de Sousa Neto** — Matrícula: **20250071231**
- **Leônidas Ferreira da Silva Serra** — Matrícula: **20250013514**

**Disciplina:** EECP0047 - TÓPICOS EM ENGENHARIA DA COMPUTAÇÃO II - INTRODUÇÃO A PROGRAMAÇÃO FUNCIONAL 

**Professor:** Prof. Dr. Sergio Souza Costa  

**Instituição:** UFMA — Universidade Federal do Maranhão  

**Semestre:** 2026.2

---

<div align="center">

**Este repositório possui fins acadêmicos.**

</div>

---