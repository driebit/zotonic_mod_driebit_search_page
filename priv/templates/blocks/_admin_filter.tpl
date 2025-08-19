{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
    {_ Filter Category _}
    <div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}


{% block widget_content %}
    <fieldset>
    <h3>{_ Settings _}</h3>
    <div class="form-group block-title">
        <label class="control-label col-md-3" for="title{{ lang_code_for_id }}">
            {_ Title _}
        </label>
        <div class="col-md-9">
            <input
                type="text"
                name="blocks[].title{{ lang_code_with_dollar }}~{{ name }}"
                value="{% if blk.title %}{{ blk.title[lang_code] }}{% endif %}"
                id="title{{ lang_code_for_id }}"
                class="form-control"
            />
        </div>
    </div>
    </fieldset>
{% endblock %}

{% block widget_content_nolang %}
    <div class="controls">
        <div class="radio">
                <div>
                    <label>
                        <input type="radio" name="blocks[].collapse~{{ name }}" {% if blk.collapse == 'collapsed' or not blk.collapse %}checked{% endif %} value="collapsed" id="collapsed">
                        {_ Show collapsed _}
                    </label>
                </div>
                <div>
                    <label>
                        <input type="radio" name="blocks[].collapse~{{ name }}" {% if blk.collapse == 'not_collapsed' %}checked{% endif %} value="not_collapsed" id="not_collapsed">
                        {_ Show expanded _}
                    </label>
                </div>
                <div>
                    <label>
                        <input type="radio" name="blocks[].collapse~{{ name }}" {% if blk.collapse == 'uncollapsible' %}checked{% endif %} value="uncollapsible" id="uncollapsible">
                        {_ Show without collapse _}
                    </label>
                </div>
            </div>
        </div>

    {% block component %}
    {% endblock %}

    {% block custom_props %}
    {% endblock %}

{% endblock %}