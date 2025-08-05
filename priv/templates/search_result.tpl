<div class="search__result">
    {% with q.id as id %}
        {% include "list/list-item.tpl" id=id %}
    {% endwith %}
</div>