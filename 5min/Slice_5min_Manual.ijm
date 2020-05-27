
for (i = 0; i <= 99; i++) {
roiManager("Select", i);
run("Clear", "slice");
}

run("8-bit");

setThreshold(0, 0);
run("Convert to Mask", "method=Default background=Light");

run("Stack to Images");



for (i = 1; i <= 100; i++) {

selectWindow("slice:" + toString(i));
saveAs("Tiff", "C:/Users/George/Desktop/CHE4180/NEW Project Material/Cropped/ROC Prep 5min/5min_Manual_slice" + toString(i) + ".tif");
close();

}
