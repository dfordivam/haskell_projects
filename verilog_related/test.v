module top();

middle m1(.d(c));
middle m1(.d(c));

endmodule

module middle(a,b,c);
leaf l1(.c(d));
endmodule


module leaf();
middle m1(.c(d));
endmodule

primitive latch(q,d);
output q;
input q;
table
//  d q
    0:0;
    1:1;
endtable
endprimitive
