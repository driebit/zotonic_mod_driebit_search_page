{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ General settings _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_i18n_tab_class %}item{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-notifications{% endblock %}

{% block widget_content %}
{% with not id or m.rsc[id].is_editable as is_editable %}
    <h3> {_ Exclude categories from search results _} </h3>
    <p> {_ Choose the categories that are excluded in the search filters. _} </p>
    <div class="checkbox">
        {% for c in m.category.tree_flat_meta %}
            <div>
                <label>
                    <input type="checkbox" name="exclude_categories[]~{{ name }}" {% if c.id|member:id.exclude_categories %}checked{% endif %} value="{{ c.id }}" id="{{ c.id }}">
                    {{ c.indent }}{{ c.id.name }} 
                </label>
            </div>
        {% endfor %}
    </div>

    <h3>{_ Results per page _}</h3>
    <p>{_ Set how many search results are shown per page. _}</p>
    <input
        type="number"
        name="page_len~{{ name }}"
        value="{{ id.page_len|default:20 }}"
        min="1"
        step="1"
        class="form-control"
        style="max-width: 7rem;"
    >

    <h3>{_ Default sorting _}</h3>
    <p>{_ Choose how search results are sorted by default. _}</p>
    <select
        name="default_sort~{{ name }}"
        class="form-control"
        style="max-width: 20rem;"
    >
        <option value="relevance" {% if not id.default_sort or id.default_sort == 'relevance' %}selected{% endif %}>
            {_ Relevance _}
        </option>
        <option value="pivot.title" {% if id.default_sort == 'pivot.title' %}selected{% endif %}>
            {_ Title _}
        </option>
        <option value="-rsc.modified" {% if id.default_sort == '-rsc.modified' %}selected{% endif %}>
            {_ Latest modified _}
        </option>
        <option value="-rsc.created" {% if id.default_sort == '-rsc.created' %}selected{% endif %}>
            {_ Publication date _}
        </option>
    </select>
{% endwith %}
{% endblock %}
