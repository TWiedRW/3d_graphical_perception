// These are filled in by R
code = "Test code";
bar_size_z = [31, 10, 56, 64, 20, 0, 51, 14, 48, 61, 45]; 
letters = ["", "\u25B2", "\u25CF", "", "", "", "", "", "", "", ""];
groups = ["A", "A", "A", "A", "A", "", "B", "B", "B", "B", "B"];

// Cumulative sum function
// https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Tips_and_Tricks#Cumulative_sum
function cumsum(values) = ([ for (a=0, b=values[0]; a < len(values); a= a+1, b=b+(values[a]==undef?0:values[a])) b] );

// Add vector values
// https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Tips_and_Tricks#Add_all_values_in_a_list
function add(v, i = 0, r = 0) = i < len(v) ? add(v, i + 1, r + v[i]) : r;

// Add vectors
// https://forum.openscad.org/adding-vectors-function-td14524.html
function addvec(v, av) = (len(v) == len(av)) ? [ for (i = [ 0 : len(v)-1 ]) v[i] + av[i] ] : [0,0,0]; 
    
// These define the [x,y] dims of the bar, in mm
bar_size_x = [for(i = [0:10]) 10];
bar_size_y = [for(i = [0:10]) 10];
    
// These define the plot margins
margins_x = [10, 10];
margins_y = [10, 10];

// This calculates the base dimensions
base_x = margins_x[0] + margins_x[1] + add(bar_size_x);
base_y = margins_y[0] + margins_y[1] + max(bar_size_y);
base_z = 10;

// These define the location of the bottom of the bar
bar_x = cumsum(bar_size_x);
bar_y = [for(i = [0:10]) margins_y[0]];
bar_z = [for(i = [0:10]) base_z];

// These define the location of the text on the bar (if applicable)
letter_x = addvec(bar_x, bar_size_x/2);
letter_y = addvec(bar_y, bar_size_x/2);
letter_z = addvec(bar_z, bar_size_z);

// Letter extrusion code
// https://files.openscad.org/examples/Basics/text_on_cube.html
font = "Liberation Sans";

module letter(l) {
	// Use linear_extrude() to make the letters 3D objects as they
	// are only 2D shapes when only using text()
    if(1!="") {
        linear_extrude(height = letter_height) {
            text(l, size = letter_size, font = font, halign = "center", valign = "center", $fn = 16);
        }
    }
}

letter_size = 5; // 5mm
letter_height = 2; // 2mm


render() {
    difference() {
        // Create the plot
        union() {
            // Base
            cube([base_x,base_y,base_z]);

            // Bars
            for(i = [0:10]) {
                translate([bar_x[i], bar_y[i], bar_z[i]])
                    cube([bar_size_x[i], bar_size_y[i], bar_size_z[i]]);
                translate([letter_x[i], letter_y[i], letter_z[i]])
                    letter(letters[i]);
            }

            // TODO: Create this based off of groups...
            labels_x = [margins_x[0]/2 + add([for(i=[0:4]) bar_x[i]])/5, margins_x[0]/2 + add([for(i=[6:10]) bar_x[i]])/5];
            labels_y = [margins_y[0]/2, margins_y[0]/2];
            labels_z = [base_z, base_z];
            labels = ["A", "B"];

            for(i = [0:(len(labels)-1)]) {
                translate([labels_x[i], labels_y[i], labels_z[i]])
                    letter(labels[i]);
            }
        };
        // Subtract off the code
        

        translate([base_x/2,base_y/2,letter_height])
            rotate([180,0,0])
            letter(code);
    }
}
