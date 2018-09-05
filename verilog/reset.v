module reset(CLK, RST);
   input CLK;
   output RST;
   
   
   reg [7:0] reset_count;
   assign RST = (reset_count == 255);
   always @(posedge CLK) begin
      if (!RST) begin
         reset_count <= (reset_count + 1);
      end
   end

endmodule
