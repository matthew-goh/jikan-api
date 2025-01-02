document.addEventListener("DOMContentLoaded", function () {
    const data = JSON.parse(document.getElementById('viewers-chart').textContent);

    // Specify the categories to display
    const categories = ['watching', 'completed', 'on_hold', 'dropped', 'plan_to_watch'];

    // Filter the data based on the selected categories
    const filteredData = categories.reduce((result, category) => {
        if (data[category] !== undefined) {
            result[category] = data[category];
        }
        return result;
    }, {});

    const labels = Object.keys(filteredData);  // Get the keys (category names)
    const values = Object.values(filteredData);  // Get the corresponding values for each category

    const numberFormatter = new Intl.NumberFormat('en-UK'); // Formatter for numbers with commas

    const ctx = document.getElementById('viewersChart').getContext('2d');
    new Chart(ctx, {
        type: 'bar',
        data: {
            labels: ['Watching', 'Completed', 'On hold', 'Dropped', 'Plan to watch'],  // Specify labels
            datasets: [
                {
                    label: 'Categories',
                    data: values,  // Values for each category
                    backgroundColor: 'rgba(3, 138, 255, 0.4)', // Color for bars
                    borderColor: 'rgba(3, 138, 255, 1)',
                    borderWidth: 1
                }
            ]
        },
        options: {
            responsive: true,
            indexAxis: 'y',  // Horizontal bars
            plugins: {
                title: {
                    display: true,
                    text: 'Viewing Status Breakdown',
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
                legend: {
                    display: false  // Hide legend
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            const value = context.raw;  // Get the value of the bar
                            return `${numberFormatter.format(value)} users`;
                        }
                    }
                }
            },
            scales: {
                x: {
                    beginAtZero: true,
                    title: {
                        display: true,
                        text: 'Users',
                        font: {
                            size: 14
                        },
                        color: '#000000'
                    }
                }
            }
        }
    });
});
