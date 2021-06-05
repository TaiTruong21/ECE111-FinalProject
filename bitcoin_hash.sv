parameter
  IDLE=5'b00000, 
  COMPUTE1=5'b10000, 
  COMPUTE2=5'b10001, COMPUTE3=5'b10010, 
  PREP11=5'b01110, PREP12=5'b01100, 
  PREP13=5'b01101, 
  PREP21=5'b01000, PREP22=5'b01001,
  PREP31=5'b01010, PREP32=5'b01011,
  WRITE=5'b00001; 

module bitcoin_hash (input              clk, reset_n, start,
                     input       [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input       [31:0] mem_read_data);

parameter num_nonces = 16;

logic [ 4:0] state;	// see params above; could have used enums instead
logic [ 4:0] wc; //write counters
logic [ 6:0] t;
logic [31:0] k1;
logic [31:0] hout[num_nonces];

parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

assign mem_clk = clk;

// instantiate SHA256 modules
genvar q;
generate
  for (q=0; q<num_nonces; q++) begin : generate_pblocks
    pblock block(
	  .clk       (clk),    // make the connections -- easy: match the port names
	  .state     (state), 
	  .t         (t), 
	  .n         (n),
	  .mem_read_data(mem_read_data),
	  .k1        (k1),
	  .hout      (hout[q]));
  end
endgenerate
//ZACK STARTED ADDING IN THIS FUNCTION HERE-------------------------------------------SAT, JUNE 5
always_ff @(posedge clk, negedge reset_n) 
  if(!reset_n) begin
    state <= IDLE;
	done  <= 0;
  end
  else
    case(state)
      IDLE:	if(start) begin
        mem_we   <= 0;
        mem_addr <= message_addr;
        t        <= 0;
        state    <= PREP11;
      end
      PREP11: begin
        state    <= PREP12;
        mem_addr <= mem_addr + 1;
      end
      PREP12: begin
	      for (int count = 0; count < num_nonces; count++) begin
			 w[count][15] <= mem_read_data;
		         h[count][0] <= 32'h6a09e667;
	     	      h[count][1] <= 32'hbb67ae85;
		      h[count][2] <= 32'h3c6ef372;
		      h[count][3] <= 32'ha54ff53a;
		      h[count][4] <= 32'h510e527f;
		      h[count][5] <= 32'h9b05688c;
		      h[count][6] <= 32'h1f83d9ab;
		      h[count][7] <= 32'h5be0cd19;
	      end //end for loop
        state <= PREP13;
        mem_addr <= mem_addr + 1;
        k1 <= k[t];
      end
	  PREP13: begin
		  for (int i = 0; i < num_nonces; i++) begin
			  for (int j = 0; j < 15; j++)
				  w[i][j] <= w[i][j + 1];
			  	  w[i][15] <= mem_read_data;
		  end
        mem_addr <= mem_addr + 1;
        state    <= COMPUTE1;
	k1       <= k[t+1];
        t        <= t + 1;
      end
      COMPUTE1: begin //START FROM HERE ==============================
        if (!(t[6] && t[0])) begin // t<65
          if (t<15) 
            mem_addr <= 
          else 
            mem_addr <= 
          k1 <= 
          t  <= 
        end 
        else begin
          t        <= 
          mem_addr <= 
          state    <= PREP21;
        end
      end
      PREP21: begin
        state    <= PREP22;
        mem_addr <= 
        k1       <= 
      end
      PREP22: begin
        mem_addr <= 
        state    <= COMPUTE2;
        k1       <= 
        t        <= 
      end
      COMPUTE2: begin
        if (!(t[6] && t[0])) begin // t<65
          if (t<2) 
            mem_addr <= 
          k1 <= 
          t <= 
        end 
        else begin
          t     <= 
          state <= PREP31;
        end
      end
      PREP31: begin
        state <= PREP32;
        k1    <= 
      end
      PREP32: begin
          state <= COMPUTE3;
          k1    <= 
          t     <= 
      end
	  COMPUTE3: begin
        if (!(t[6] && t[0])) begin // t<65
          k1 <= 
          t  <= 
        end 
        else begin
          wc    <= 
          state <= WRITE;
        end
      end
      WRITE: begin
        if (wc < num_nonces) begin
          mem_we         <= 
          mem_addr       <= 
          mem_write_data <= 
          wc             <= 
        end 
        else begin
          state <= IDLE;
          done  <= 1;
        end
      end
    endcase
endmodule
