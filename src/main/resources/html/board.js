
function Board( array ){
    this.pieces = array;

    this.rows = this.pieces.length;
    this.columns = this.pieces[0].length;
}

Board.prototype.pieceAt = function(r,c) {
    if( r < 0 || r >= this.rows ){
        throw "r fuera de rango:" + r;
    }
    if( c < 0 || r >= this.columns ){
        throw "c fuera de rango:" + c;
    }

    return this.pieces[r][c];
}

Board.prototype.render = function(elem){

    elem = $(elem);

    var table = $('<table class="board"></table>');

    for( var r = 0 ; r < this.rows ; r += 1 ){

        var tr = $("<tr></tr>");
        table.append( tr );

        for( var c = 0 ; c < this.columns ; c += 1 ){
            var piece = this.pieceAt(r,c);
            var td = $("<td></td>");
            piece.render( td );
            tr.append(td);
        }
    }

    elem.append(table);
}

