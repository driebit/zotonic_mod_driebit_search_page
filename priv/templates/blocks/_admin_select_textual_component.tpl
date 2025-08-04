<h3>{_ Component _}</h3>
<div class="controls">
    <div class="radio">
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'checkboxes' or not blk.component %}checked{% endif %} value="checkboxes" id="checkboxes">
                {_ Checkboxes (multiple options selectable) _}
            </label>
        </div>
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'dropdown' %}checked{% endif %} value="dropdown" id="dropdown">
                {_ Dropdown (single option selectable) _}
            </label>
        </div>
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'multiselect' %}checked{% endif %} value="multiselect" id="multiselect">
                {_ Multiselect with search _}
            </label>
        </div>
    </div>
</div>

