{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
    {_ Full text search _}<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_i18n_tab_class %}item{% endblock %}


{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-full_text{% endblock %}
{% block widget_title %}Full text search{% endblock %}

{% block widget_content %}
{% with not id or m.rsc[id].is_editable as is_editable %}
<fieldset class="form-horizontal">
    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #placeholder }}{{ lang_code_for_id }}">{_ placeholder _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #placeholer }}{{ lang_code_for_id }}" name="placeholder{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : id.translation[lang_code].placeholder : id.placeholder | brlinebreaks }}</textarea>
        </div>
    </div>
</fieldset>
{% endwith %}
{% endblock %}