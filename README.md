# Berrydocs - Knowledge Hub <img src="media/berrylogo.png" align="right" width="75" alt="" />

The repository at hand hosts the Knowledge Hub. The website is deployed from the `/docs` folder of the `main` branch of the repository, so any changes not reflected in the `/docs` folder of the `main` branch will not be reflected on the live website.

The website is built using *Quarto*, which means the `/docs` folder is populated automatically using the features of *Quarto* and should not be edited manually. In order to work on the website, you will need to have [*Git*](https://git-scm.com/downloads) and [*Quarto*](https://quarto.org/docs/get-started/) installed.

This document is a guide how to modify the contents and build the website using *Quarto* and finally how to deploy the site using *Github pages*. 

## Modify Content

1. `git clone` the repository.
: This can be done using the *Git* manager inbuilt in **R Studio** or **Positron**, or using any other *Git* manager. The command is 

    ```
    git clone https://github.com/BerryConsultants/berrydocs
    ```
    
2. Modify existing pages.
: To modify an existing page, you need to modify the corresponding *Quarto* file. The folder structure in the repository generally (but not exclusively) follows the folder structure in the `/docs` folder (i.e. on the website). 

    As an example, imagine you want to edit the list of videos and webinars. The corresponding website is [`https://docs.berryconsultants.com/introduction/webinars.html`](https://docs.berryconsultants.com/introduction/webinars.html). The corresponding *Quarto* file you need to modify is `/introduction/webinars.qmd`. For a guide on the *Quarto* Markdown syntax, please refer to the [*Quarto* website](https://quarto.org). 

3. Add new pages.
: Whenever you want to add new pages, the workflow differs slightly depending on the contents of `/_quarto.yml`. This file governs the navigation bar and sidebar structure on the website. 

    Oftentimes, all the contents of a given folder will be displayed automatically in alphabetic order (by *Quarto* filename), such as in `/introduction/gettingStarted/` (this is achieved with a relevant line of `auto: introduction/gettingStarted/*.qmd` in the `.yml` file). In this case, feel free to duplicate any of the `.qmd` files in this folder and change its contents, the rest is done automatically.

    In case you are creating a new folder, you need to make sure that the new files are picked up by `/_quarto.yml` (i.e. they are actually displayed somewhere on the website). Please follow the existing structure of creating navigation bar and sidebar entries to make the new page visible.

## Build Site

In any terminal, navigate to the location to where you cloned the `berrydocs` repository. You can now build the website by running

```
quarto render
```

which re-generates the `/docs` folder from the `.qmd` files following a set of rules specified in the `.yml` and `.css` files.

If you want to preview the website, you may run 

```
quarto preview
```

## Deploy Site

When you are done making changes and building the new website locally, please `git push` your changes to the main branch.

The website is deployed using [*Github pages*](https://pages.github.com). The settings in the *Github* repository automatically deploy the contents of the `/docs` folder of the `main` branch to [`docs.berryconsultants.com`](docs.berryconsultants.com). Any changes to the contents of the `/docs` folder on the `main` branch will be picked up immediately by *Github* once changes have been git pushed, the website will be rebuilt and the changes should be visible within a few minutes. Please note that depending on the nature of the changes, they might take a little longer to be picked up (at most 24h). 

By default, the website deploys to [BerryConsultants.github.io/berrydocs](BerryConsultants.github.io/berrydocs). Having a `CNAME` in place means an alias is created for [docs.berryconsultants.com](docs.berryconsultants.com).
