{% extends "blocks/_admin_filter.tpl" %}

{% block component %}
    {% include "blocks/_admin_select_date_component.tpl" blk=blk blocks=blocks name=name %}
{% endblock %}

{% block custom_props %}
    <h3>{_ Date type _}</h3>
    <div class="controls">
        <div class="radio">
            <div>
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == 'publication_date' or not blk.date_prop %}checked{% endif %} value="publication_date" id="publication_date">
                    {_ Publication date _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == 'date' %}checked{% endif %} value="date" id="date">
                    {_ Event date _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == 'modification_date' %}checked{% endif %} value="modification_date" id="modification_date">
                    {_ Modification date _}
                </label>
            </div>
            <div style="display:flex; algin-items:center;">
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == "custom" %}checked{% endif %} value="custom" id="custom">
                    {_ Custom _}
                </label>
                <div class="col-md-6">
                    <input
                        type="text"
                        name="blocks[].custom_date_prop~{{ name }}"
                        value="{% if blk.custom_date_prop %}{{ blk.custom_date_prop }}{% endif %}"
                        id="custom_date_prop"
                        class="form-control"
                    />
                </div>
            </div>
        </div>
    </div>
{% endblock %}
