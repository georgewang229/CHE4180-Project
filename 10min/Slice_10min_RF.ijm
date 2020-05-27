
for (i = 0; i <= 105; i++) {
roiManager("Select", i);
run("Clear", "slice");
}

run("8-bit");

setThreshold(1, 255);
run("Convert to Mask", "method=Default background=Light");

run("Stack to Images");



for (i = 1; i <= 100; i++) {

selectWindow("slice:" + toString(i));
saveAs("Tiff", "C:/Users/George/Desktop/CHE4180/NEW Project Material/Cropped/ROC Prep/10min_RF_slice" + toString(i) + ".tif");
close();

}
