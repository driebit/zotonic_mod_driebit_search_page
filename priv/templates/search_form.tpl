<div class="c-search">
    <div class="c-full-text-search">
        <form class="do_forminit"
            data-onsubmit-topic="model/location/post/qlist/submit"
            data-oninput-topic="model/location/post/qlist/submit"
            method="GET">
            <input  class="c-full-text-search__searchbar" type="text" name="qtext" value="" placeholder="zoeken">
        </form>
    </div>
    <div class="c-search-filters">
        <form class="do_forminit"
        data-onsubmit-topic="model/location/post/qlist/submit"
        data-oninput-topic="model/location/post/qlist/submit"
        method="GET">
            {% for blk in m.rsc[`search_page`].blocks %}
                {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join blk=blk id=id %}
            {% endfor %}
        </form>
    </div>
    <div class="c-search-results">
        {% include "search_results.tpl" %}
    </div>
</div>