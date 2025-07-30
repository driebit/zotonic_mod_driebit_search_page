<h3>Component</h3> 
    <div class="controls">
    <div class="radio">
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'checkboxes' or not blk.component %}checked{% endif %} value="checkboxes" id="checkboxes">
                {_ Checkboxes (meerdere opties selecteerbaar) _}
            </label>
        </div>
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'dropdown' %}checked{% endif %} value="dropdown" id="dropdown">
                {_ Dropdown (één optie selecteerbaar) _}
            </label>
        </div>
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'multiselect' %}checked{% endif %} value="multiselect" id="dropdown">
                {_ Multiselect met zoek _}
            </label>
        </div>
    </div>
</div>

