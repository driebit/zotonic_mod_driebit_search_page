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
{% endwith %}
{% endblock %}