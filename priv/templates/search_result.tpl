<div class="search__result">
    {% with q.id as id %}
        {% catinclude "list/list-item.tpl" id %}
    {% endwith %}
</div>