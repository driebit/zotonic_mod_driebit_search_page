{% optional include "_translation_init_languages.tpl" %}

{% catinclude "_admin_edit_basics.tpl" id show_header=false %}

{% all catinclude "_admin_edit_content.tpl" id %}

{% catinclude "_admin_edit_blocks.tpl" id %}