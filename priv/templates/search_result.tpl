{% with q.id as id %}
    {% catinclude "list/list-item.tpl" id parent="search_page" %}
{% endwith %}