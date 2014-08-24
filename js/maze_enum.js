{$,Parser hint: pure}
$(function() {
	$('#maze').svg({
		onLoad: function() {
			create_maze('maze', $('#difficulty').val());
		},
		settings: {viewBox: '0.5 0.5 500 500'}
	});

	$('#difficulty').change(function() {
		$('#difficulty_display').text($(this).val());
		create_maze('maze', $(this).val());
	});
});
