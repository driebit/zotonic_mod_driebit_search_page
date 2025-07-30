<h3>Component</h3> 
    <div class="controls">
    <div class="radio">
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'fixed_ranges' or not blk.component %}checked{% endif %} value="fixed_ranges" id="fixed_ranges">
                {_ Knoppen met tijdsperiodes _}
            </label>
        </div>
        <div>
            <label>
                <input type="radio" name="blocks[].component~{{ name }}" {% if blk.component == 'calendar' %}checked{% endif %} value="calendar" id="calendar">
                {_ Kalender (één datum selecteerbaar) _}
            </label>
        </div>
    </div>
</div>