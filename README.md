# Classification and Clustering of Low Birth Weight Mortality Using Global Health and Economic Data

An R Shiny application for the classification and clustering of low birth weight mortality, using global health and economic data.

## Features

- Interactive exploratory data analysis (EDA)
- Classification model comparison (Decision Tree, Naive Bayes)
- LIME-based model interpretability
- Hierarchical clustering and 2D mapping
- Automated HTML report generation

## Demo Video

Watch the demo and overview here:  
[https://youtu.be/Qgy3AlAgZ7w](https://youtu.be/Qgy3AlAgZ7w)

## Getting Started

1. Clone the repository or download the source files.
2. Ensure you have R and RStudio installed.
3. Install required packages as listed in the [`project.rmd`](./project.rmd) and [`app.R`](./app.R).
4. **Run [`project.rmd`](./project.rmd) first** to process the data and generate [`app.RData`](./app.RData).
5. **After `app.RData` is generated, run [`app.R`](./app.R) in RStudio** to launch the Shiny app.
6. The app will automatically load data from the generated `app.RData` file.

## Project Structure

- [`app.R`](./app.R) – Main Shiny application file
- [`project.rmd`](./project.rmd) – Analysis and reporting in RMarkdown
- [`project.html`](./project.html) – Generated report
- [`app.RData`](./app.RData) – Data for use in app (auto-generated)
- [`README.md`](./README.md) – Project information

## Author

Cedrus Dang

## License

For academic, research, and educational use only.
