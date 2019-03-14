

function Piece( inTop, inNorth, inEast, inSouth, inWest, inBottom ) {
    this.inTop = inTop;
    this.inNorth = inNorth;
    this.inEast = inEast;
    this.inSouth = inSouth;
    this.inWest = inWest;
    this.inBottom = inBottom;
}


Piece.prototype.render = function(elem){
    elem = $(elem);

    var table = $("<table/></table>");

    var tr = $("<tr></tr>");

    tr.append( $("<td/>") );
    tr.append( $('<td class="piece ' + this.inNorth + '"/>') );
    tr.append( $("<td/>") );
    table.append( tr );

    tr = $("<tr></tr>");
    tr.append( $('<td class="piece ' + this.inWest + '"/>') );
    tr.append( $('<td class="piece ' + this.inTop + '"/>') );
    tr.append( $('<td class="piece ' + this.inEast + '"/>') );
    table.append( tr );

    tr = $("<tr></tr>");
    tr.append( $("<td/>") );
    tr.append( $('<td class="piece ' + this.inSouth + '"/>') );
    tr.append( $("<td/>") );
    table.append( tr );

    elem.append(table);
}
