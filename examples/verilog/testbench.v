// Working example, to be used as inspiration for generated code.

module counter(clk, rst, count);
   input clk;
   input rst;
   output [7:0] count;
   assign count = count_reg;
   reg [7:0] count_reg;
   
   always @(posedge clk, negedge rst) begin
      if (rst == 0) begin
         count_reg <= 0;
      end
      else begin
         count_reg <= count_reg + 1;
      end
   end
endmodule

module counter_tb;

   reg clk;
   reg rst;
   wire [7:0] count; 
   
   counter U0 (clk, rst, count);

   reg [7:0] v = 123;
   
   
   
   initial begin

      //$increment(v);
      
      clk <= 0;
      rst <= 0;
      #1 rst <= 1;

      $seq_to(count, clk, rst);
      $seq_from(v);
      
      repeat (20) @(posedge clk);
      $finish;
   end
   
   always @(posedge clk) begin
      $display("%d", count);
      $seq_tick;
      
   end
   
   always 
     #5  clk = ~clk;

endmodule
