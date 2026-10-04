
document.addEventListener("DOMContentLoaded", function() {

// Check if calculator elements exist before initializing
const calculatorElement = document.querySelector(".calculator-grid-bvr");

if (!calculatorElement) {
    // Exit if calculator elements are not present on this page
    return;
}
    
  let debounceTimer;
  let valueChart = null;

function handleInput(input) {
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    if (input.id === "monthlyRent" || input.id === "homePrice") {
      formatCurrency(input);
    } else {
      formatPercent(input);
    }
    calculateDecision();
  }, 1000);
}

function formatCurrency(input) {
  // Remove any non-digit characters except decimal point
  let value = input.value.replace(/[^\d.]/g, "");
  // Convert to number and format
  let number = parseFloat(value);
  if (!isNaN(number)) {
    input.value = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      minimumFractionDigits: 0,
      maximumFractionDigits: 0
    }).format(number);
  }
}

function formatPercent(input) {
  // Remove any non-digit characters except decimal point
  let value = input.value.replace(/[^\d.]/g, "");
  // Convert to number and format
  let number = parseFloat(value);
  if (!isNaN(number)) {
    input.value = number.toFixed(2) + "%";
  }
}

function parseCurrency(value) {
  return parseFloat(value.replace(/[$,]/g, ""));
}

function parsePercent(value) {
  return parseFloat(value.replace("%", ""));
}

function calculateMonthlyMortgage(principal, annualRate, years) {
  const monthlyRate = annualRate / 12 / 100;
  const numberOfPayments = years * 12;
  return principal * monthlyRate * Math.pow(1 + monthlyRate, numberOfPayments) 
  / (Math.pow(1 + monthlyRate, numberOfPayments) - 1);
}

function calculateRemainingMortgage(principal, annualRate, totalMonths, monthsPassed) {
    const monthlyRate = annualRate / 12 / 100;
    const monthlyPayment = calculateMonthlyMortgage(principal, annualRate, totalMonths/12);
    
    let remainingBalance = principal * Math.pow(1 + monthlyRate, monthsPassed) - 
        monthlyPayment * ((Math.pow(1 + monthlyRate, monthsPassed) - 1) / monthlyRate);
    
    return Math.max(0, remainingBalance);
}

function calculateDecision() {
    // Get input values
    const inflation = parsePercent(document.getElementById("inflation").value) / 100;
    const initialMonthlyRent = parseCurrency(document.getElementById("monthlyRent").value);
    const propertyTax = parsePercent(document.getElementById("propertyTax").value) / 100;
    const maintenance = parsePercent(document.getElementById("maintenance").value) / 100;
    const insurance = parsePercent(document.getElementById("insurance").value) / 100;
    const downpaymentPercent = parsePercent(document.getElementById("downpayment").value) / 100;
    const interestRate = parsePercent(document.getElementById("interestRate").value);
    const initialHomePrice = parseCurrency(document.getElementById("homePrice").value);
    
    // Calculate key values
    const downpaymentAmount = initialHomePrice * downpaymentPercent;
    const closingCosts = initialHomePrice * 0.03; // 3% closing costs
    const loanAmount = initialHomePrice - downpaymentAmount;
    const monthlyMortgage = calculateMonthlyMortgage(loanAmount, interestRate, 30);
    
    // Initialize starting values
    let portfolioValue = downpaymentAmount + closingCosts;
    const portfolioGrowthRate = parsePercent(document.getElementById("portfolioGrowth").value) / 100;
    const totalAnnualReturn = (1 + portfolioGrowthRate) - 1;
    const monthlyReturn = Math.pow(1 + totalAnnualReturn, 1/12) - 1;
    const monthlyInflation = Math.pow(1 + inflation, 1/12) - 1;

    // Calculate month by month
    let currentMonthlyRent = initialMonthlyRent;
    let currentHomePrice = initialHomePrice;
    
    // Display initial monthly costs
    document.getElementById("monthlyMortgage").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round(monthlyMortgage));
    
    document.getElementById("monthlyPropertyTax").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round((initialHomePrice * propertyTax) / 12));
    
    document.getElementById("monthlyMaintenance").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round((initialHomePrice * maintenance) / 12));
    
    document.getElementById("monthlyInsurance").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round((initialHomePrice * insurance) / 12));
    
    document.getElementById("totalMonthlyCost").textContent = new Intl.NumberFormat("en-US", {
        style: "currency",
        currency: "USD",
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(Math.round(monthlyMortgage + 
        (initialHomePrice * propertyTax) / 12 + 
        (initialHomePrice * maintenance) / 12 + 
        (initialHomePrice * insurance) / 12));
        
     let monthlyData = [];
      let labels = [];
      let portfolioValues = [];
      let homeEquity = [];
    
    for (let i = 0; i <= 360; i++) {
        // Update home price and related costs with inflation
        currentHomePrice *= (1 + monthlyInflation);
        currentMonthlyRent *= (1 + monthlyInflation);
        
        // Calculate remaining mortgage and equity
        const remainingMortgage = calculateRemainingMortgage(loanAmount, interestRate, 360, i);
        const currentEquity = currentHomePrice - remainingMortgage;
        
        let currentMonthlyPropertyTax = (currentHomePrice * propertyTax) / 12;
        let currentMonthlyMaintenance = (currentHomePrice * maintenance) / 12;
        let currentMonthlyInsurance = (currentHomePrice * insurance) / 12;
        
        // Calculate current months housing cost
        const totalMonthlyHousingCost = monthlyMortgage + 
          currentMonthlyPropertyTax + 
          currentMonthlyMaintenance + 
          currentMonthlyInsurance;

  // Calculate and apply monthly investment
  const monthlyInvestment = totalMonthlyHousingCost - currentMonthlyRent;
  portfolioValue = portfolioValue * (1 + monthlyReturn) + monthlyInvestment;
  
      if (i % 12 === 0) {
          let year = Math.floor(i/12);
          labels.push(year);
          portfolioValues.push(Math.round(portfolioValue));
          homeEquity.push(Math.round(currentEquity));
      }
    }
    
  // Test formatting function
    function formatYAxis(value) {
        try {
            return "$" + value.toLocaleString();
        } catch (e) {
            console.error("Error formatting:", e);
            return value;
        }
    }  
  
 // Update the chart
        if (valueChart) {
            valueChart.destroy();
        }
        
        const ctx = document.getElementById("valueChart").getContext("2d");
        valueChart = new Chart(ctx, {
            type: "line",
                  data: {
                    labels: Array.from({length: 31}, (_, i) => i),  // This will create labels 0-30
                    datasets: [{
                        label: "Portfolio Value",
                        data: portfolioValues,
                        borderColor: "#4CAF50",
                        backgroundColor: "#4CAF50",
                        fill: false,
                        tension: 0.4,
                        borderWidth: 2
                    }, {
                        label: "Home Equity",
                        data: homeEquity,
                        borderColor: "#2196F3",
                        backgroundColor: "#2196F3",
                        fill: false,
                        tension: 0.4,
                        borderWidth: 2
                    }]
                },
            options: {
                    responsive: true,
                    maintainAspectRatio: false,
                   tooltips: {
                        callbacks: {
                            title: function(tooltipItems, data) {
                                return "Year = " + tooltipItems[0].xLabel;
                            },
                            label: function(tooltipItem, data) {
                                let value = tooltipItem.yLabel;
                                return data.datasets[tooltipItem.datasetIndex].label + ": " + 
                                    new Intl.NumberFormat("en-US", {
                                        style: "currency",
                                        currency: "USD",
                                        minimumFractionDigits: 0,
                                        maximumFractionDigits: 0
                                    }).format(value);
                            }
                        }
                    },
                    plugins: {
                        legend: {
                            position: "top",
                            align: "center",
                            labels: {
                                usePointStyle: true,
                                boxWidth: 16,
                                boxHeight: 16,
                                padding: 20,
                                color: "#000000",
                                font: {
                                    size: 14
                                }
                            }
                        }
                    },
                    scales: {
                        yAxes: [{
                            ticks: {
                                callback: function(value) {
                                    return new Intl.NumberFormat("en-US", {
                                        style: "currency",
                                        currency: "USD",
                                        minimumFractionDigits: 0,
                                        maximumFractionDigits: 0
                                    }).format(value);
                                },
                                beginAtZero: true
                            }
                        }],
                        xAxes: [{
                            scaleLabel: {
                                display: true,
                                labelString: "Year"
                            }
                        }]
                    }
                }
        });
  
  // Final home value is already calculated in currentHomePrice
  const homeValue = currentHomePrice;
  
  // Update results display
  document.getElementById("portfolioValue").textContent = new Intl.NumberFormat("en-US", {
          style: "currency",
          currency: "USD",
          minimumFractionDigits: 0,
          maximumFractionDigits: 0
      }).format(Math.round(portfolioValue));
      
      document.getElementById("finalHomeValue").textContent = new Intl.NumberFormat("en-US", {
          style: "currency",
          currency: "USD",
          minimumFractionDigits: 0,
          maximumFractionDigits: 0
      }).format(Math.round(homeValue));
  
    document.getElementById("finalDecision").textContent = 
      portfolioValue > homeValue ? "RENT" : "BUY";
    document.getElementById("finalDecision").style.color = 
      portfolioValue > homeValue ? "#4CAF50" : "#2196F3";
  }

// Calculate initial results when page loads
    calculateDecision();

    // Add event listeners to all inputs
    document.querySelectorAll("input").forEach(input => {
        input.addEventListener("input", () => handleInput(input));
    });
});
