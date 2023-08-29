#' Render a Quarto multilingual project
#'
#' @importFrom rlang `%||%`
#'
#' @details babelquarto expects a book/website folder with
#' each qmd/Rmd present in as many languages as needed,
#' with the same basename but,
#' - once with only `.qmd` as extension for the main language,
#' - once with `.es.qmd` (using the language code) for each other language.
#'
#' You also need to register the language in the configuration file,
#' see [babelquarto::register_main_language()]
#' and [babelquarto::register_further_languages()]:
#'
#' ```yaml
#' babelquarto:
#'   mainlanguage: 'en'
#'   languages: ['es', 'fr']
#' ```
#'
#' @importFrom rlang `%||%`
#'
#' @param book_path Path where the book source is located
#' @param website_path Path where the website source is located
#' @param site_url Base URL of the book/website.
#'
#' @export
#'
#' @examples
#' directory <- withr::local_tempdir()
#' quarto_multilingual_book(parent_dir = directory, book_dir = "blop")
#' render_book(file.path(directory, "blop"))
#' \dontrun{
#' if (require("servr") && rlang::is_interactive()) {
#'   servr::httw(file.path(directory, "blop", "_book"))
#' }
#' }
#'
#' @rdname render
render_book <- function(book_path = ".", site_url = NULL) {
  render(path = book_path, site_url = site_url, type = "book")
}
#' @export
#' @rdname render
render_website <- function(website_path = ".", site_url = NULL) {
  render(path = website_path, site_url = site_url, type = "website")
}
render <- function(path = ".", site_url = NULL, type = c("book", "website")) {
  # configuration ----
  config <- file.path(path, "_quarto.yml")
  config_contents <- yaml::read_yaml(config)

  if (is.null(site_url)) {
    if (nzchar(Sys.getenv("BABELQUARTO_TESTS_URL")) || !on_ci()) {
      site_url <- site_url %||% config_contents[[type]][["site-url"]] %||% ""
      site_url <- sub("/$", "", site_url)
    } else {
      # no end slash
      # for deploy previews
      # either root website (Netlify deploys)
      # or something else
      site_url <- Sys.getenv("BABELQUARTO_CI_URL", "")
    }
  }

  output_dir <- config_contents[["project"]][["output-dir"]] %||%
    switch(
      type,
      book = "_book",
      website = "_site"
    )

  language_codes <- config_contents[["babelquarto"]][["languages"]]
  if (is.null(language_codes)) {
    cli::cli_abort("Can't find {.field babelquarto/languages} in {.field _quarto.yml}")
  }
  main_language <- config_contents[["babelquarto"]][["mainlanguage"]]
  if (is.null(main_language)) {
    cli::cli_abort("Can't find {.field babelquarto/mainlanguage} in {.field _quarto.yml}")
  }

  output_folder <- file.path(path, output_dir)
  if (fs::dir_exists(output_folder)) fs::dir_delete(output_folder)

  # pre-defined style for language link / button
  # edition: (XX edition)
  # version: Version in XX (the default from PR 11 from maelle)
  nav_style  <- config_contents[["babelquarto"]][["nav-style"]]
  if (is.null(nav_style)) {
    nav_style <- "links"
  }    
  
  display_current_language <- config_contents[["babelquarto"]][["display-current-language"]]
  if (is.null(display_current_language)) {
    nav_style <- "none"
  }    
  
 
  # render book ----
  withr::with_dir(path, {
    quarto::quarto_render(as_job = FALSE)
  })

  purrr::walk(
    language_codes,
    render_quarto_lang,
    path = path,
    output_dir = output_dir,
    type = type
  )

  # Add the language switching link to the sidebar ----
  ## For the main language ----
  purrr::walk(
    language_codes,
    ~ purrr::walk(
      fs::dir_ls(output_folder, glob = "*.html"),
      add_link,
      main_language = main_language,
      language_code = .x,
      site_url = site_url,
      type = type,
	  nav_style = nav_style,
	  display_current_language = display_current_language
    )
  )

  ## For other languages ----
  for (other_lang in language_codes) {
    languages_to_add <- c(main_language, language_codes[language_codes != other_lang])
    purrr::walk(
      languages_to_add,
      ~ purrr::walk(
        fs::dir_ls(file.path(output_folder, other_lang), glob = "*.html"),
        add_link,
        main_language = main_language,
        language_code = .x,
        site_url = site_url,
        type = type,
	    nav_style = nav_style,		
		display_current_language = display_current_language
        )
    )
  }

}

render_quarto_lang <- function(language_code, path, output_dir, type) {

  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(path, temporary_directory)
  project_name <- fs::path_file(path)

  config <- yaml::read_yaml(file.path(temporary_directory, project_name, "_quarto.yml"))
  config$lang <- language_code
  config[[type]][["title"]] <- config[[sprintf("title-%s", language_code)]] %||% config[[type]][["title"]]
  config[[type]][["description"]] <- config[[sprintf("description-%s", language_code)]] %||% config[[type]][["description"]]

  if (type == "book") {
    config[[type]][["author"]] <- config[[sprintf("author-%s", language_code)]] %||% config[[type]][["author"]]
    config[["book"]][["chapters"]] <- purrr::map(
      config[["book"]][["chapters"]],
      use_lang_chapter,
      language_code = language_code,
      book_name = project_name,
      directory = temporary_directory
    )
    yaml::write_yaml(config, file.path(temporary_directory, project_name, "_quarto.yml"))
  }

  if (type == "website") {
    yaml::write_yaml(config, file.path(temporary_directory, project_name, "_quarto.yml"))
    config_text <- brio::read_lines(file.path(temporary_directory, project_name, "_quarto.yml"))
    paths_m <- gregexpr("[a-zA-Z\\-\\_\\.]*\\.qmd", config_text)
    paths <- unlist(regmatches(config_text, paths_m))
    # for not to replace twice
    paths <- paths[order(nchar(paths), decreasing = TRUE)]

    for (qmd_path in paths) {
      target_path <- fs::path_ext_set(
        file.path(temporary_directory, project_name, qmd_path),
        sprintf(".%s.qmd", language_code)
      )
      if (fs::file_exists(target_path)) {
        regex_path <- gsub("\\.", "\\\\.", qmd_path)
        regex_target_path <- gsub("\\.", "\\\\.", fs::path_file(target_path))
        config_text <- gsub(regex_path, regex_target_path, config_text)
      }
    }

    brio::write_lines(config_text, file.path(temporary_directory, project_name, "_quarto.yml"))
  }

  # fix for Boolean that is yes and should be true
  config_lines <- brio::read_lines(file.path(temporary_directory, project_name, "_quarto.yml"))
  config_lines[grepl("code-link", config_lines)] <- sub("yes", "true", config_lines[grepl("code-link", config_lines)])
  config_lines[grepl("reader-mode", config_lines)] <- sub("yes", "true", config_lines[grepl("reader-mode", config_lines)])
  config_lines[grepl("toc", config_lines)] <- sub("yes", "true", config_lines[grepl("toc", config_lines)])
  brio::write_lines(config_lines, file.path(temporary_directory, project_name, "_quarto.yml"))

  # Render language book
  withr::with_dir(file.path(temporary_directory, project_name), {
    quarto::quarto_render(as_job = FALSE)
  })

  # Copy it to local not temporary _book/<language-code>
  fs::dir_copy(
    file.path(temporary_directory, project_name, output_dir),
    file.path(path, output_dir, language_code)
  )

}

use_lang_chapter <- function(chapters_list, language_code, book_name, directory) {
  withr::local_dir(file.path(directory, book_name))

    original_chapters_list <- chapters_list

    if (is.list(chapters_list)) {
      # part translation
      chapters_list[["part"]] <- chapters_list[[sprintf("part-%s", language_code)]] %||%
        chapters_list[["part"]]

      # chapters translation

      chapters_list$chapters <- gsub("\\.Rmd", sprintf(".%s.Rmd", language_code), chapters_list$chapters)
      chapters_list$chapters <- gsub("\\.qmd", sprintf(".%s.qmd", language_code), chapters_list$chapters)
      if (any(!fs::file_exists(chapters_list$chapters))) {
        chapters_not_translated <- !fs::file_exists(chapters_list$chapters)
        fs::file_move(
          original_chapters_list$chapters[chapters_not_translated],
          gsub("\\.Rmd", sprintf(".%s.Rmd", language_code) ,
            gsub(
              "\\.qmd", sprintf(".%s.qmd", language_code),
              original_chapters_list$chapters[chapters_not_translated])
            )
        )
      }
    } else {
      chapters_list <- gsub("\\.Rmd", sprintf(".%s.Rmd", language_code), chapters_list)
      chapters_list <- gsub("\\.qmd", sprintf(".%s.qmd", language_code), chapters_list)
      if (!fs::file_exists(file.path(directory, book_name, chapters_list))) {
        fs::file_move(
          original_chapters_list,
          chapters_list
        )
      }
    }

    chapters_list
}

add_current_language_indicator <- function(html, main_language = main_language, language_code, site_url, type, nav_style) {

  # https://daroczig.github.io/logger/
  logger::log_debug('add_current_language_indicator: begin - nav_style = {nav_style} / main_language = {main_language} / language_code = {language_code}')
  
    
	  # <header id="quarto-header" class="headroom fixed-top"><nav class="navbar navbar-expand-lg navbar-dark "><div class="navbar-container container-fluid">
      # <div class="navbar-brand-container">
    # <a class="navbar-brand" href="./index.html">    <<<<<< navbar_brand
    # <span class="navbar-title">babelsite local</span>
    # </a>
    # <span class="navbar-title">FR</span>	
  # </div>
	

	language_indicator_exists <- (length(xml2::xml_find_first(html, "//span[@id='current-language']")) > 0)
	
	if ( !language_indicator_exists )
	{
		navbar_brand <- xml2::xml_find_first(html, "//a[@class='navbar-brand']")	
		navbar_brand_length <- length(xml2::xml_find_first(html, "//a[@class='navbar-brand']"))
		logger::log_debug('add_current_language_indicator: language_indicator_exists = {language_indicator_exists}')
		
		xml2::xml_add_sibling(
			navbar_brand,
			"span",
			sprintf("[%s]", language_code),
			class = "navbar-title",
			id = "current-language",
			# .where = 0
			.where = "after"
		  )	
	}
	else {
		# logger::log_debug('language_indicator_exists is TRUE')
	}
	
}
  


add_link <- function(path, main_language = main_language, language_code, site_url, type, nav_style, display_current_language) {

  logger::log_debug('add_link: begin - main_language = {main_language} / language_code = {language_code} / path = {path} / display_current_language = { display_current_language } / nav_style = {nav_style}' )
  
  html <- xml2::read_html(path)
  
  # get the language of page from the page path
  # ./docs/de/about.fr.html => fr  
  # ./docs/fr/about.fr.html => fr
  # ./docs/fr/index.html => en
  # ./docs/index.fr.html => fr
  # ./docs/index.html => en
  
  if ( display_current_language == "left" )
  {
	  total_length <- nchar (path)  
	  sub_path <- sub("\\.[a-z][a-z]\\.html", "", path )
		# ./docs/de/about.fr.html => ./docs/de/about 
		# ./docs/index.html => ./docs/index.html
	  partial_length <- nchar (sub_path)
	  
	  # when no language code in filename, partial_length == total_length
	  if ( total_length == partial_length )
	  {
		# no language code on file => main_language
		page_code <- main_language
	  }
	  else
	  {
		page_code <- substr ( path, partial_length + 2, partial_length + 3)
	  }
	  
	  add_current_language_indicator(html, main_language, page_code, site_url, type, nav_style)
  }
  else
  {
	logger::log_debug('add_link: display_current_language = { display_current_language }' )
  }
  
  
  if (language_code == main_language) {
    new_path <- sub("\\..*\\.html", ".html", basename(path))
    href <- sprintf("%s/%s", site_url, new_path)
  } else {
    base_path <- sub("\\..*\\.html", ".html", basename(path))
    new_path <- fs::path_ext_set(base_path, sprintf(".%s.html", language_code))
    href <- sprintf("%s/%s/%s",site_url, language_code, new_path)
  }

  
  if ( nav_style == "dropdown" ) {
	tongue_button <- sprintf("%s", toupper(language_code))  
  } else {
	tongue_button <- sprintf("Version in %s", toupper(language_code))
  }
  
  
  if (type == "book") {

    left_sidebar <- xml2::xml_find_first(html, "//div[@class='sidebar-menu-container']")

    languages_links_div_exists <- (length(xml2::xml_find_first(html, "//div[@id='languages-links']")) > 0)

    if (!languages_links_div_exists) {
      xml2::xml_add_sibling(
        left_sidebar,
        "div",
        class = "sidebar-menu-container",
        id = "languages-links",
        .where = "before"
      )
      xml2::xml_add_child(
        xml2::xml_find_first(html, "//div[@id='languages-links']"),
        "ul",
        class = "list-unstyled mt-1",
        id = "language-links-ul"
      )
    }

    languages_links <- xml2::xml_find_first(html, "//ul[@id='language-links-ul']")



    xml2::xml_add_child(
      languages_links,
      "a",
      tongue_button,
      class = "toc-action",
      href = href,
      id = sprintf("language-link-%s", language_code)
    )

    just_added_link <- xml2::xml_find_first(html, sprintf("//a[@id='language-link-%s']", language_code))
    xml2::xml_add_parent(just_added_link, "li", id = sprintf("language-link-li-%s", language_code))

    just_added_link_item <- xml2::xml_find_first(html, sprintf("//li[@id='language-link-li-%s']", language_code))
    xml2::xml_add_child(just_added_link_item, "span", " ", .where = 0)
    xml2::xml_add_child(just_added_link_item, "i", class = "bi bi-globe2", .where = 0)
  } else {
    # type <> book (ie : website, ...)

	
	if ( nav_style == "dropdown" ) {
		
		# for links, buttons on the right side of navbar (before the search bar)
		ul_navbar <- xml2::xml_find_first(html, "//ul[@class='navbar-nav navbar-nav-scroll ms-auto']")
		
		
		# drop_menu_exists if this is the second call for the current language under treatment
		drop_menu_exists <- (length(xml2::xml_find_first(html, "//li[@id='tongue_drop_menu']")) > 0)
		
		if (!drop_menu_exists )
		{
			# add drop menu that will allow switch between the different language 
			tongue_drop_menu <- xml2::xml_add_child(ul_navbar, "li", id ="tongue_drop_menu", class="nav-item dropdown")
			tongue_anchor <- xml2::xml_add_child(tongue_drop_menu, "a", id ="navbarDropdown", class="nav-link dropdown-toggle", `data-bs-toggle` = "dropdown", role="button", `aria-expanded`="false")
			bi_globe <- xml2::xml_add_child(tongue_anchor, "i", "", class="bi bi-globe2")
			tongue_ul <- xml2::xml_add_child(tongue_drop_menu, "ul", id ="tongue_ul", class="dropdown-menu", `aria-labelledby`="navbarDropdown")
			# xml2::xml_add_child(bi_globe, "span", class="menu-text")		
		}
		else
		{
			tongue_ul <- xml2::xml_find_first(html, "//ul[@id='tongue_ul']")
		}

		
	# <ul class="navbar-nav navbar-nav-scroll ms-auto">

			# <li id="tongue_drop_menu" class="nav-item dropdown" >
			  # <a id="navbarDropdown" class="nav-link dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown" aria-expanded="false">
				# <i class="bi bi-globe2"></i>
			  # </a>
			  # <ul id="tongue_ul" class="dropdown-menu" aria-labelledby="navbarDropdown">
				# <li id="language-link-li-fr" class="nav-item compact">
				#	<a class="nav-link" href="http://127.0.0.1:4321/fr/index.fr.html" id="language-link-fr">
				#		<span class="menu-text">FR</span>
				#	</a>
				# </li>
				# <li id="language-link-li-de" class="nav-item compact"><a class="nav-link" href="http://127.0.0.1:4321/de/index.de.html" id="language-link-de"><span class="menu-text">DE</span></a></li>
			  # </ul>
			  
			# </li>
		
		
		
		tongue_li <- xml2::xml_add_child(tongue_ul, "li", id = sprintf("language-link-li-%s", language_code), class="nav-item compact")
		
		tongue_link <- xml2::xml_add_child(
		  tongue_li,
		  "a",
		  # style = "text-decoration: none; color:inherit;",
		  id = sprintf("language-link-%s", language_code),	  
		  class = "nav-link",
		  href = href,
		  .where = 0
		)
		
		# simple, display code (no icon)
		tongue_span <- xml2::xml_add_child(
			tongue_link, 
			"span",
			language_code,
			id = sprintf("language-span-%s", language_code),	  
			class="menu-text"
		)
	  }
	  else # nav_style = links
	  {
		logger::log_debug('add_link: nav_style = { nav_style }' )	  
		navbar <- xml2::xml_find_first(html, "//div[@id='navbarCollapse']")
		xml2::xml_add_child(
		  navbar,
		  "a",
		  style = "text-decoration: none; color:inherit;",
		  sprintf("Version in %s - ", toupper(language_code)),
		  class = "nav-item",
		  href = href,
		  id = sprintf("language-link-%s", language_code),
		  .where = 0
		)		
	  }
  }

  xml2::write_html(html, path)
}

# as in testthat
on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}
