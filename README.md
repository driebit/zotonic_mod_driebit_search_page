# zotonic_mod_driebit_search_page


This module makes it easy to implement a search page in different projects. The added filters in the sidebar are configurable in the CMS, which makes it easy to adjust for different projects. 

The module is suitable for zotonic-1.x sites with a template based setup. Kenniscloud.nl is the first project to use the module, the general shape of the module is modeled to their wishes. 

## Local installation 
- Copy the module into your apps directory within zotonic 
- Run 'task' in the main directory of the module
- Turn on the module
- Install data fixtures adds the search page. 

## Usage 
On the 'search_page' resource, you can add filters by adding blocks, and adjust general settings in the resource. 
The module is a work in progress, not every client whish will fit into the current capabilities of the module. To exend the module, preferable adjust the module to make it more configurable instead of overriding funtionality in the project.
Styling is often project specific, and can be overridden in the project itself. 

## Implementation notes
The module uses elm for the frontend of the search page. This was chosen over the standard zotonic search with live reloading templates, because it was hard to include javascript within the live templates. Using elm also gives full control over the search params, making easier to fully customize the search query.

At the time of writing the module is an MVP, there are a lot of things that would still be good to implement:

- Sorting: configurable 
- Add search params in url 
- Integrate calendar module 
- Use cat instead of specific page
- Extend pagination view 
- Custom filter block 
- View options to display filters/searchbar on top or in the sidebar 
- Add micro animations 
- Configure live reloading or not
- Add facetted search option

