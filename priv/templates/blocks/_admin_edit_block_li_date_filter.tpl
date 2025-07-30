{% extends "blocks/_admin_filter.tpl" %}

{% block component %}
    {% include "blocks/_admin_select_date_component.tpl" blk=blk blocks=blocks name=name %}
{% endblock %}

{% block custom_props %}
    <h3>Type datum</h3> 
        <div class="controls">
        <div class="radio">
            <div>
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == 'publication_date' or not blk.date_prop %}checked{% endif %} value="publication_date" id="publication_date">
                    {_ Publicatiedatum _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == 'date' %}checked{% endif %} value="date" id="date">
                    {_ Event datum _}
                </label>
            </div>
        </div>
    </div>
{% endblock %}
