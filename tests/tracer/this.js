var __VOLUME, __RED, __ID, __BOTTOM, __TOP, __LEFT, __FACTORY, color, bottom, left, __device;

__VOLUME=8;
__RED="red";
__ID=12342;
__BOTTOM=1.1;
__TOP=0.1;
__LEFT=0.5;


__FACTORY = function(arg1, arg2){
	this.volume=__VOLUME;
	color=__RED;
	this.id=arg1;
	bottom=arg2;
	this.top=arguments[2];
	left=arguments[3];
};

__device = new __FACTORY(__ID, __BOTTOM, __TOP, __LEFT);