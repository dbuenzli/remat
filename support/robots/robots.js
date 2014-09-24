function markup () { return window.document.documentElement.innerHTML; }
var page = new WebPage();
page.settings.loadImages = false;
page.open(phantom.args[0], function (status) 
{
    //TODO add doctype and enclose in <html>
    console.log(page.evaluate(markup));
    phantom.exit();
});
