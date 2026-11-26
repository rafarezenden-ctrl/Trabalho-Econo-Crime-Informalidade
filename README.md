# Informalidade e crimes contra o patrimônio: evidências do roubo e furto de veículos nos municípios paulistas

Davi Lopes Schiavo e Rafael Nogueira Rezende — Econometria I, FGV EESP, 2026.2

Arquivos de replicação do artigo: base de dados e script em R.

## Conteúdo

- `Codigo_completo_Final.R`: monta a base, gera as figuras e estima as regressões e os testes.
- `Dados/brutos/`: os 11 arquivos de dados brutos (8 tabelas do SIDRA/IBGE, crimes da SSP-SP, frota do SENATRAN e regiões metropolitanas do IBGE).
- `Dados/base_final.csv`: a base final usada nas estimações (645 municípios). O script a recria a partir dos dados brutos.

## Como rodar

1. Baixar o repositório pelo botão **Code > Download ZIP** e descompactar numa pasta (o código não roda de dentro do .zip).
2. Abrir `Codigo_completo_Final.R` no RStudio (R 4.2 ou mais novo).
3. Instalar os pacotes, uma vez só, no console:
   `install.packages(c("lmtest", "sandwich", "sf", "geobr"))`
4. Session > Set Working Directory > To Source File Location.
5. Rodar tudo com Ctrl + Shift + Enter (Source with Echo).

## Observações

- O mapa precisa de internet: o pacote geobr baixa a malha dos municípios do IBGE.
- O código cria sozinho as pastas `Figuras/` e `Resultados/`.
