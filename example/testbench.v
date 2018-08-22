// Working example, to be used as inspiration for generated code.

module counter(clk, rst, count);
   input clk;
   input rst;
   output [3:0] count;
   assign count = count_reg;
   reg [3:0] count_reg;
   
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
   wire [3:0] count; 
   
   counter U0 (.clk    (clk),
               .rst    (rst), 
               .count  (count) 
               );

   initial begin
      clk <= 0;
      rst <= 0;
      #100 rst <= 1;
      $display("start");
      repeat (20) @(posedge clk);
      $display("finish");
      $finish;
   end
   
   always @(posedge clk) begin
      $display("%h", count);
   end
   
   always 
     #5  clk = ~clk;

endmodule
