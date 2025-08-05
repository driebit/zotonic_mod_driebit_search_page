{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
    {_ Filter Category _}<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}

{% block widget_content %}
{% endblock %}

{% block widget_content_nolang %}

    {% block base_props %}
        <h3>{_ Settings _}</h3>
        <div class="form-group row">
            <label class="control-label col-md-3" for="title">{_ Title _}</label>
            <div class="col-md-9">
                <input type="text" name="blocks[].title~{{ name }}" value="{{ blk.title }}" id="title" class="form-control" />
            </div>
        </div>
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
                        <input type="radio" name="blocks[].collapse~{{ name }}" {% if blk.collapse == 'uncollapsable' %}checked{% endif %} value="uncollapsable" id="uncollapsable">
                        {_ Show without collapse _}
                    </label>
                </div>
            </div>
        </div>
    {% endblock %}

    {% block component %}
    {% endblock %}

    {% block custom_props %}
    {% endblock %}

{% endblock %}