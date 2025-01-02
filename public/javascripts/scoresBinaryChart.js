document.addEventListener("DOMContentLoaded", function () {
    const data = JSON.parse(document.getElementById('scores-chart').textContent);

    let negative = 0;
    let positive = 0;

    data.forEach(function(score) {
        if (score.score <= 5) {
            negative += score.votes;
        } else {
            positive += score.votes;
        }
    });
    const totalVotes = negative + positive;

    const ctx = document.getElementById('scoresBinaryChart').getContext('2d');
    new Chart(ctx, {
        type: 'bar',
        data: {
            labels: [''], // No labels needed for a single bar
            datasets: [
                {
                    label: 'Negative (1-5)',
                    data: [negative],
                    backgroundColor: '#F88379',
                    stack: 'total',
                    barThickness: 30,
                    borderColor: '#636363', // Border color
                    borderWidth: 1,
                    borderSkipped: false // Ensure border appears at the bottom
                },
                {
                    label: 'Positive (6-10)',
                    data: [positive],
                    backgroundColor: '#39c9bb',
                    stack: 'total',
                    barThickness: 30,
                    borderColor: '#636363', // Border color
                    borderWidth: 1,
                    borderSkipped: false // Ensure border appears at the bottom
                }
            ]
        },
        options: {
            responsive: true,
            indexAxis: 'y', // Switch the chart to horizontal orientation
            plugins: {
                title: {
                    display: true,
                    text: 'Binary Score Sentiment',
                    font: {
                        size: 16,
                        weight: 'bold'
                    },
                    color: '#2c3e50',
                    padding: {
                        top: 20,
                        bottom: 10
                    }
                },
                tooltip: {
                    callbacks: {
                        label: function(context) {
                            const value = context.raw;
                            const percentage = ((value / totalVotes) * 100).toFixed(1);
                            return `${context.dataset.label}: ${value.toLocaleString()} votes (${percentage}%)`;
                        }
                    }
                }
            },
            scales: {
                x: {
                    stacked: true, // Enable stacking for the x-axis
                    display: false, // Hide the x-axis labels and grid
                    min: 0,
                    max: totalVotes
                },
                y: {
                    stacked: true, // Enable stacking for the y-axis
                    display: false, // Hide the y-axis labels and grid
                }
            }
        }
    });
});
