document.addEventListener("DOMContentLoaded", function () {
    const data = JSON.parse(document.getElementById('scores-chart').textContent);

    let groupedVotes = {
        "Poor (1-4)": 0,
        "Average (5-6)": 0,
        "Good (7-8)": 0,
        "Excellent (9-10)": 0
    };

    data.forEach(function(score) {
        if (score.score >= 1 && score.score <= 4) {
            groupedVotes["Poor (1-4)"] += score.votes;
        } else if (score.score >= 5 && score.score <= 6) {
            groupedVotes["Average (5-6)"] += score.votes;
        } else if (score.score >= 7 && score.score <= 8) {
            groupedVotes["Good (7-8)"] += score.votes;
        } else if (score.score >= 9 && score.score <= 10) {
            groupedVotes["Excellent (9-10)"] += score.votes;
        }
    });

    const labels = Object.keys(groupedVotes);
    const votes = Object.values(groupedVotes);

    const numberFormatter = new Intl.NumberFormat('en-UK'); // Formatter for numbers with commas

    const ctx = document.getElementById('scoresPieChart').getContext('2d');
    new Chart(ctx, {
        type: 'pie',
        data: {
            labels: labels,
            datasets: [{
                label: 'Votes by Group',
                data: votes,
                backgroundColor: ['#FF6384', '#FFCE56', '#36A2EB', '#4BC0C0'],
                hoverOffset: 4
            }]
        },
        options: {
            responsive: true,
            plugins: {
                legend: {
                    labels: {
                        boxWidth: 15,
                        boxHeight: 15,
                        padding: 10
                    }
                },
                title: {
                    display: true,
                    text: 'Score Sentiment Breakdown',
                    font: {
                        size: 16,
                        weight: 'bold'
                    },
                    color: '#2c3e50',
                    padding: {
                        top: 20,
                        bottom: 20
                    }
                },
                tooltip: {
                    callbacks: {
                        label: function(context) {
                            // Format tooltip to include percentage and votes
                            const totalVotes = context.dataset.data.reduce((a, b) => a + b, 0);
                            const value = context.raw;
                            const percentage = ((value / totalVotes) * 100).toFixed(1);
                            return `${numberFormatter.format(value)} votes (${percentage}%)`;
                        }
                    }
                }
            }
        }
    });
});
