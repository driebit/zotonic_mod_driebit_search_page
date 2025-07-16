{% if blk.displaymode == 'dropdown' %}
    {% include "search/_dropdown.tpl" names=blk.show_categories filter=filter %}
{% else %}
    {% include "search/_checkbox_fieldset.tpl" label="type" filter=filter names=blk.show_categories %}
{% endif %}